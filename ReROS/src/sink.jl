# The Rerun sink + single-writer log pump (DESIGN.md §5, DESIGN-MAPPING.md §2.2).
#
# Mappers and the graph recorder are producers: they build a `LogItem` and `emit!`
# it onto a bounded channel. One writer task drains the channel and is the *only*
# code that touches the `RecordingStream` — that exclusive ownership (not thread
# pinning) is what keeps Rerun's per-thread current-time coherent (DESIGN.md §5.2).

"""Per-sample timeline stamps applied before a log (DESIGN-MAPPING.md §2.1)."""
struct TimeStamps
    recv_ns::Int64                  # wall clock at receipt (always)
    ros_ns::Union{Int64,Nothing}    # from a message header.stamp, or nothing
    seq::Int64                      # monotonic recorder counter
end

"""Which Rerun timelines a sink stamps. Disabled timelines are skipped per item."""
struct Timelines
    recv::Bool
    ros::Bool
    seq::Bool
end
Timelines(; recv::Bool=true, ros::Bool=true, seq::Bool=true) = Timelines(recv, ros, seq)

"""One queued log: an entity path, a Rerun archetype value, its stamps, and whether
it is static (logged off-timeline) or temporal."""
struct LogItem
    path::String
    payload::Rerun.Archetype
    times::TimeStamps
    static::Bool
end

# ── Sink specs (config-driven; DESIGN.md §5.1) ────────────────────────────────
abstract type SinkSpec end

"""Write the recording to a `.rrd` file at `path`."""
struct Save <: SinkSpec
    path::String
    Save(path::AbstractString) = new(String(path))
end

"""Stream to a running viewer over gRPC."""
struct Grpc <: SinkSpec
    url::String
    Grpc(; url::AbstractString="rerun+http://127.0.0.1:9876/proxy") = new(String(url))
end

"""Spawn a local Rerun viewer and stream to it (needs the `rerun` executable)."""
struct Spawn <: SinkSpec end

# ── The sink ──────────────────────────────────────────────────────────────────
"""
    Sink(app_id; recording_id, sinks, timelines, capacity, drop_when_full)

Owns a Rerun `RecordingStream`, a bounded channel, and the single writer task that
drains it. Producers call [`emit!`](@ref); `close(sink)` drains, flushes, and frees.
"""
mutable struct Sink
    rec::Rerun.RecordingStream
    chan::Channel{LogItem}
    capacity::Int
    timelines::Timelines
    drop_when_full::Bool
    seq::Threads.Atomic{Int64}
    dropped::Threads.Atomic{Int64}
    write_errors::Threads.Atomic{Int64}
    dropped_by_path::Dict{String,Int}
    lock::ReentrantLock
    writer::Task
    closed::Threads.Atomic{Bool}

    function Sink(app_id::AbstractString;
                     recording_id::Union{AbstractString,Nothing}=nothing,
                     sinks::AbstractVector{<:SinkSpec}=SinkSpec[Save("recording.rrd")],
                     timelines::Timelines=Timelines(),
                     capacity::Integer=4096,
                     drop_when_full::Bool=true)
        rec = Rerun.RecordingStream(app_id; recording_id=recording_id)
        _attach_sinks!(rec, sinks)
        chan = Channel{LogItem}(capacity)
        werr = Threads.Atomic{Int64}(0)
        writer = Threads.@spawn _writer_loop(chan, rec, timelines, werr)
        Base.errormonitor(writer)
        return new(rec, chan, Int(capacity), timelines, drop_when_full,
                   Threads.Atomic{Int64}(0), Threads.Atomic{Int64}(0), werr,
                   Dict{String,Int}(), ReentrantLock(), writer, Threads.Atomic{Bool}(false))
    end
end

Base.show(io::IO, s::Sink) = print(io, "Sink(", Base.n_avail(s.chan), "/", s.capacity,
                                      " queued, ", s.dropped[], " dropped, ",
                                      s.write_errors[], " write-errors)")

function _attach_sinks!(rec::Rerun.RecordingStream, specs)
    rsinks = Rerun.LogSink[]
    spawn_requested = false
    for s in specs
        if s isa Save
            push!(rsinks, Rerun.FileSink(s.path))
        elseif s isa Grpc
            push!(rsinks, Rerun.GrpcSink(s.url))
        elseif s isa Spawn
            spawn_requested = true
        else
            error("ReROS: unknown sink spec $(s)")
        end
    end
    if spawn_requested
        if isempty(rsinks)
            Rerun.spawn(rec)                       # spawn the viewer and stream to it
        else
            # `spawn`/`set_sinks` each REPLACE the active sink set, so calling both drops
            # one. Launch the viewer process, then route to it AND the file/grpc sinks in a
            # single set_sinks (the spawned viewer listens on the default gRPC port).
            Rerun.spawn()
            push!(rsinks, Rerun.GrpcSink())
            Rerun.set_sinks(rec, rsinks...)
        end
    else
        isempty(rsinks) || Rerun.set_sinks(rec, rsinks...)
    end
    return rec
end

@inline function _apply_times(rec::Rerun.RecordingStream, t::TimeStamps, tl::Timelines)
    tl.recv && Rerun.set_time(rec, "recv_time", t.recv_ns; kind=:timestamp)
    tl.ros && t.ros_ns !== nothing && Rerun.set_time(rec, "ros_time", t.ros_ns::Int64; kind=:timestamp)
    tl.seq && Rerun.set_time(rec, "log_seq", t.seq; kind=:sequence)
    return nothing
end

# The sole toucher of `rec`. Runs until the channel is closed and drained. Each item is
# isolated: a per-item `reset_time` clears the prior item's thread-local timeline indices
# (so a static item is truly off-timeline and a headerless item never inherits a stale
# ros_time), and a try/catch keeps one bad sample from killing the pump (DESIGN.md §5.2).
function _writer_loop(chan::Channel{LogItem}, rec::Rerun.RecordingStream, tl::Timelines,
                      werr::Threads.Atomic{Int64})
    for item in chan
        try
            Rerun.reset_time(rec)
            item.static || _apply_times(rec, item.times, tl)
            Rerun.log(rec, item.path, item.payload; inject_time = !item.static)
        catch err
            Threads.atomic_add!(werr, Int64(1))
            @error "ReROS: writer dropped a log item" path=item.path exception=(err, catch_backtrace()) maxlog=10
        end
    end
    return nothing
end

_now_ns() = round(Int64, time() * 1e9)   # wall clock (Unix epoch ns)

"""Next monotonic log-sequence id. One per logical sample (a whole message shares one)."""
next_seq!(sink::Sink) = Threads.atomic_add!(sink.seq, Int64(1))

"""Low-level enqueue with explicit stamps (used by the mapper `emit!`, §map.jl)."""
_push!(sink::Sink, path::String, payload::Rerun.Archetype, times::TimeStamps, static::Bool) =
    _offer!(sink, LogItem(path, payload, times, static))

"""
    emit!(sink, path, payload; recv_ns=now, ros_ns=nothing, static=false) -> Bool

Queue a log (auto-assigning a fresh seq). Returns `false` (and counts a drop) when the
channel is full and `drop_when_full`, or when the sink is closed; `true` once queued.
"""
function emit!(sink::Sink, path::AbstractString, payload::Rerun.Archetype;
               recv_ns::Int64=_now_ns(), ros_ns::Union{Int64,Nothing}=nothing,
               static::Bool=false)
    return _push!(sink, String(path), payload, TimeStamps(recv_ns, ros_ns, next_seq!(sink)), static)
end

function _offer!(sink::Sink, item::LogItem)
    sink.closed[] && return false
    # Non-blocking-ish drop: check buffered count first. A small race (fills between
    # the check and `put!`) only costs a brief block, never a deadlock.
    if sink.drop_when_full && Base.n_avail(sink.chan) >= sink.capacity
        _record_drop!(sink, item.path)
        return false
    end
    try
        put!(sink.chan, item)
    catch err
        err isa InvalidStateException && return false   # raced a close
        rethrow()
    end
    return true
end

function _record_drop!(sink::Sink, path::AbstractString)
    Threads.atomic_add!(sink.dropped, Int64(1))
    @lock sink.lock sink.dropped_by_path[path] = get(sink.dropped_by_path, path, 0) + 1
    return nothing
end

"""Total items dropped due to a full channel."""
dropped(sink::Sink) = sink.dropped[]

"""Total items the writer failed to log (a per-item exception was caught and counted)."""
write_errors(sink::Sink) = sink.write_errors[]

"""
    close(sink; timeout_sec=2.0)

Stop accepting items, drain the queue, flush Rerun, and free the stream. Idempotent.
"""
function Base.close(sink::Sink; timeout_sec::Real=2.0)
    sink.closed[] && return sink
    sink.closed[] = true
    close(sink.chan)                       # writer finishes remaining items, then returns
    # The writer is errormonitored; a dead writer must not abort the flush/close below,
    # or the .rrd is left unflushed and the stream handle leaks (DESIGN.md §5.3).
    wait(sink.writer; throw=false)
    try
        Rerun.flush(sink.rec; timeout_sec=timeout_sec)
    catch err
        @error "ReROS: flush on close failed" exception=err
    end
    Rerun.close(sink.rec)
    return sink
end
