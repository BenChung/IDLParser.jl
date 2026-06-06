# §10 parameters: the transactional commit/rollback + dynamic dict, all
# in-process (the server holds `node === nothing`, so reads/transactions/sugar/
# events run without the wire surface). The schema is declared at file scope —
# `@parameters` generates an immutable struct + a `descriptors` method.

using ROSNode: @parameters, ParameterServer, ParameterDescriptor, descriptors,
               transaction, setproperties, setproperties!, set_parameter!,
               parameter, parameter_names, declared_names, dynamic_parameters,
               on_parameter_event, ParameterRejection, ParameterEventBatch,
               readonly, validate,
               PARAMETER_DOUBLE, PARAMETER_STRING, PARAMETER_BOOL, PARAMETER_NOT_SET,
               parameter_type, get_parameters, set_parameters_atomically,
               describe_parameters

@parameters struct PlannerParams
    "max speed in m/s"  max_speed::Float64 = 5.0 ∈ 0.0..10.0
    mode::String = "auto" ∈ ("auto", "manual")
    gain::Float64 = 1.0
    frame::String = "map" |> readonly
    use_sim_time::Bool = false
end

# Cross-field rule (ROS2's on-set callback): manual mode forbids a huge gain.
ROSNode.validate(p::PlannerParams) =
    (p.mode == "manual" && p.gain > 100.0) &&
        throw(ParameterRejection("gain too high for manual mode"))

@testset "parameters" begin
    @testset "schema generation: descriptors + types" begin
        ds = descriptors(PlannerParams)
        @test length(ds) == 5
        byname = Dict(d.name => d for d in ds)
        @test byname[:max_speed].type === Float64
        @test byname[:max_speed].ptype == PARAMETER_DOUBLE
        @test byname[:max_speed].constraint == (0.0, 10.0)
        @test byname[:max_speed].description == "max speed in m/s"
        @test byname[:mode].constraint == ("auto", "manual")
        @test byname[:frame].read_only === true
        @test byname[:gain].constraint === nothing
        @test parameter_type(Bool) == PARAMETER_BOOL
        @test parameter_type(String) == PARAMETER_STRING
        @test_throws ArgumentError parameter_type(Vector{Float32})
    end

    @testset "typed reads off the live struct" begin
        s = ParameterServer(nothing, PlannerParams())
        @test s.max_speed == 5.0
        @test s.mode == "auto"
        @test parameter(s, :gain) == 1.0
        @test declared_names(s) == (:max_speed, :mode, :gain, :frame, :use_sim_time)
        @test_throws ArgumentError s.nonexistent
    end

    @testset "transaction commits the whole candidate" begin
        s = ParameterServer(nothing, PlannerParams())
        committed = transaction(s) do p
            p.max_speed = 9.0
            p.mode = "manual"
            @test p.max_speed == 9.0          # reads see pending overrides
        end
        @test committed isa PlannerParams
        @test s.max_speed == 9.0
        @test s.mode == "manual"
    end

    @testset "constraint violation rejects + rolls back (free)" begin
        s = ParameterServer(nothing, PlannerParams())
        @test_throws ParameterRejection transaction(s) do p
            p.gain = 42.0                     # legal on its own
            p.max_speed = 99.0                # out of [0,10] ⇒ reject whole txn
        end
        @test s.max_speed == 5.0              # nothing committed
        @test s.gain == 1.0                   # the legal sibling rolled back too
    end

    @testset "choice-set constraint" begin
        s = ParameterServer(nothing, PlannerParams())
        @test_throws ParameterRejection (s.mode = "turbo")
        @test s.mode == "auto"
        s.mode = "manual"
        @test s.mode == "manual"
    end

    @testset "read-only blocks runtime sets (but startup override is allowed)" begin
        s = ParameterServer(nothing, PlannerParams())
        @test_throws ParameterRejection (s.frame = "odom")
        @test s.frame == "map"
        # an idempotent re-set of a read-only field to its current value is a no-op
        @test transaction(s) do p; p.frame = "map"; end isa PlannerParams
        @test s.frame == "map"
        # startup override via the ctor bypasses the runtime gate
        s2 = ParameterServer{PlannerParams}(nothing; overrides=(frame="odom",))
        @test s2.frame == "odom"
    end

    @testset "cross-field validate hook" begin
        s = ParameterServer(nothing, PlannerParams())
        @test_throws ParameterRejection transaction(s) do p
            p.mode = "manual"
            p.gain = 500.0
        end
        @test s.mode == "auto"                # rolled back
    end

    @testset "single-statement sugar + setproperties!" begin
        s = ParameterServer(nothing, PlannerParams())
        s.max_speed = 7.5
        @test s.max_speed == 7.5
        setproperties!(s, (max_speed=3.0, gain=2.0))
        @test s.max_speed == 3.0 && s.gain == 2.0
        set_parameter!(s, :gain, 4.0)
        @test s.gain == 4.0
        # pure value-level setproperties (no server)
        v2 = setproperties(PlannerParams(), (mode="manual",))
        @test v2.mode == "manual" && v2.max_speed == 5.0
    end

    @testset "/parameter_events fires one batch per committed transaction" begin
        s = ParameterServer(nothing, PlannerParams())
        batches = ParameterEventBatch[]
        on_parameter_event(s) do b; push!(batches, b); end
        transaction(s) do p
            p.max_speed = 8.0
            p.gain = 3.0
        end
        @test length(batches) == 1
        b = batches[1]
        @test b.changed[:max_speed] == 8.0
        @test b.previous[:max_speed] == 5.0
        @test b.changed[:gain] == 3.0
        # a no-op transaction (set to current value) fires nothing
        transaction(s) do p; p.max_speed = 8.0; end
        @test length(batches) == 1
    end

    # §7: the trigger condition the `use_sim_time → /clock` hook keys on. A committed
    # change to `use_sim_time` lands in the event batch — `_emit_parameter_event!` fires
    # the hook off exactly this. The hook itself early-returns here (`node === nothing`);
    # the live Context wiring is exercised in parameters_live.jl.
    @testset "use_sim_time change reaches the event batch (hook trigger)" begin
        s = ParameterServer(nothing, PlannerParams())
        batches = ParameterEventBatch[]
        on_parameter_event(s) do b; push!(batches, b); end
        transaction(s) do p; p.use_sim_time = true; end
        @test length(batches) == 1
        @test haskey(batches[1].changed, :use_sim_time)
        @test batches[1].changed[:use_sim_time] === true
        @test batches[1].previous[:use_sim_time] === false
        transaction(s) do p; p.use_sim_time = true; end   # no-op: no batch, hook wouldn't re-fire
        @test length(batches) == 1
    end

    @testset "dynamic (undeclared) params gated by allow_undeclared" begin
        off = ParameterServer(nothing, PlannerParams())
        @test_throws ArgumentError dynamic_parameters(off)
        @test_throws ArgumentError transaction(off) do p; p.undeclared = 1; end

        on = ParameterServer(nothing, PlannerParams(); allow_undeclared=true)
        transaction(on) do p
            p.max_speed = 6.0       # declared
            p.extra = "hi"          # dynamic
        end
        @test on.max_speed == 6.0
        @test dynamic_parameters(on)[:extra] == "hi"
        @test parameter(on, :extra) == "hi"
        @test :extra in parameter_names(on)
        # declared names still come first, in schema order
        @test parameter_names(on)[1:5] ==
              [:max_speed, :mode, :gain, :frame, :use_sim_time]
    end

    @testset "reflection handlers (get / set-atomically / describe)" begin
        s = ParameterServer(nothing, PlannerParams())
        # `map` over a tuple of names yields a tuple, preserving the request shape.
        @test get_parameters(s, (:max_speed, :mode)) == (5.0, "auto")
        @test get_parameters(s, (:nope,)) == (nothing,)
        ok, reason = set_parameters_atomically(s, ((:max_speed, 2.0), (:gain, 9.0)))
        @test ok === true && reason == ""
        @test s.max_speed == 2.0 && s.gain == 9.0
        bad_ok, bad_reason = set_parameters_atomically(s, ((:max_speed, 50.0),))
        @test bad_ok === false
        @test occursin("out of range", bad_reason)
        @test s.max_speed == 2.0          # atomic: rejected set didn't apply
        ds = describe_parameters(s, (:frame, :unknown))
        @test ds[1].read_only === true
        @test ds[2].ptype == PARAMETER_NOT_SET   # NOT_SET for an unknown name
    end

    # Client-side pure logic (the wire path is exercised by examples/parameters.jl).
    @testset "client: pure helpers" begin
        # set-input normalization → name => value pairs.
        @test ROSNode._param_pairs((a = 1, b = 2)) == [:a => 1, :b => 2]
        @test ROSNode._param_pairs([:a => 1, :b => 2]) == [:a => 1, :b => 2]
        @test ROSNode._param_pairs(Dict(:a => 1)) == [:a => 1]

        # ParameterType ↔ Julia type round-trip + the wire-byte guard.
        for T in (Bool, Int64, Float64, String, Vector{UInt8}, Vector{Bool},
                  Vector{Int64}, Vector{Float64}, Vector{String})
            @test ROSNode._param_julia_type(parameter_type(T)) === T
        end
        @test ROSNode._param_julia_type(PARAMETER_NOT_SET) === Nothing
        @test ROSNode._ptype(2) === ROSNode.PARAMETER_INTEGER
        @test ROSNode._ptype(99) === PARAMETER_NOT_SET          # out of range → NOT_SET

        # Wire-descriptor decode preserves name/type/ptype/read_only/description.
        d0 = ParameterDescriptor(:speed, Float64, PARAMETER_DOUBLE, "spd", nothing, true, 1.0)
        d1 = ROSNode._from_wire_descriptor(ROSNode._to_descriptor(d0))
        @test (d1.name, d1.type, d1.ptype, d1.read_only, d1.description) ==
              (:speed, Float64, PARAMETER_DOUBLE, true, "spd")

        # _ClientDraft: pending overrides coerce to the field type, read through to base.
        d = ROSNode._ClientDraft{PlannerParams}(PlannerParams(), Dict{Symbol, Any}())
        d.max_speed = 9                       # Int → Float64
        @test d.max_speed === 9.0             # reads the pending override
        @test d.mode == "auto"                # reads through to the base
        @test_throws ArgumentError (d.nonexistent = 1)
        @test setproperties(PlannerParams(), NamedTuple(d.overrides)).max_speed == 9.0
    end
end
