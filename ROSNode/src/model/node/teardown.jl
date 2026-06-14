# ── entity teardown ───────────────────────────────────────────────────────────

"""
    close(entity::Entity)

Undeclare the entity. Idempotent.

- Close the data route — for a Subscription this stops the consumer task's iteration.
- Withdraw the liveliness token.
- Drop it from the discovery index.
- Detach any pattern-layer wiring (`entity.wire`).
"""
function Base.close(e::Entity)
    (@atomicswap e.open = false) || return nothing   # single-winner close latch
    ctx = e.node.context

    # Data route first so the consumer task exits via FIFO disconnect rather than being
    # killed mid-dispatch: undeclaring the subscriber disconnects its channel and the
    # consumer's recv loop ends cleanly. Then withdraw liveliness, then drop the index.
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
client while keeping the node alive:

- Undeclare its route.
- Withdraw its liveliness token.
- Drop it from the discovery index.

To tear down every entity still tracked at that point, use `close(node)` instead.

The entity is removed from `node.entities` first, then closed. `dispose` is a
no-op that leaves the entity open when `node` does not track `entity`:

- It is owned by another node.
- It was already disposed.

Returns `nothing`.
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
