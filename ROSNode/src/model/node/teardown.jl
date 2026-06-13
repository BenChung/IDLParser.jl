# ── entity teardown ───────────────────────────────────────────────────────────

"""
    close(entity::Entity)

Undeclare the entity: close the data route (which stops the consumer task's
iteration for a Subscription), withdraw the liveliness token, drop it from the
discovery index, and detach any pattern-layer wiring (`entity.wire`). Idempotent.
"""
function Base.close(e::Entity)
    (@atomicswap e.open = false) || return nothing
    ctx = e.node.context

    # Close the data route first: undeclaring the subscriber disconnects its FIFO
    # channel, so the consumer task's `for sample in …` loop terminates cleanly.
    if e._route !== nothing
        try
            close(e._route)
        catch err
            @error "close(entity): closing data route failed" exception=(err, catch_backtrace())
        end
        e._route = nothing
    end

    # Pattern-layer wiring (queryable / querier / pending tables) shares our
    # lifecycle; close it if it's close-able.
    if e.wire !== nothing
        try
            applicable(close, e.wire) && close(e.wire)
        catch err
            @error "close(entity): closing pattern wiring failed" exception=(err, catch_backtrace())
        end
        e.wire = nothing
    end

    if e._lv_token !== nothing
        try
            close(e._lv_token)
        catch err
            @error "close(entity): withdrawing liveliness token failed" exception=(err, catch_backtrace())
        end
        e._lv_token = nothing
    end
    remove_endpoint!(ctx, e.lv_key)
    _forget_lost_tracker!(e)        # drop any message-lost state (no-op if none)
    unregister_local_subscription!(e)   # drop from the intra-process registry + stop its worker (no-op if not registered)
    nothing
end

"""
    dispose(node::Node, entity::Entity)

Close `entity` and remove it from `node`'s tracked set, releasing it before the
node itself is closed. Use this to reclaim a single publisher/subscription/service/
client (undeclaring its route, withdrawing its liveliness token, and dropping it
from the discovery index) while keeping the node alive; otherwise `close(node)`
tears down every entity still tracked at that point.

The entity is removed from `node.entities` first, then closed. If `node` does not
track `entity` (it is owned by another node, or was already disposed), `dispose`
is a no-op: the entity is left open. Returns `nothing`.
"""
function dispose(node::Node, e::Entity)
    @lock node.lock begin
        i = findfirst(===(e), node.entities)
        i === nothing && return nothing
        deleteat!(node.entities, i)
    end
    close(e)
    nothing
end
