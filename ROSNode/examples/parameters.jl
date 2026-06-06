# Parameter server + remote client — a Julian port of hiroz's `parameters/client`.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/parameters.jl           # both, one process (default)
#     julia --project=. ROSNode/examples/parameters.jl server    # serve only (Ctrl-C to stop)
#     julia --project=. ROSNode/examples/parameters.jl client    # drive a remote (needs a server)
#
# A node declares a typed parameter schema baked in via `Node(ctx, name, Schema)`;
# that wires the six standard `rcl_interfaces` parameter services, so the node is
# driveable uniformly by any ROS2 parameter client — rclcpp, rclpy, or the Hiroz
# master — and by our own `ParameterClient`. Unlike the other examples this one is
# self-contained: the parameter service types are vendored, so it needs no sourced
# ROS2 install, just a router.
#
# Interop: with a matching router + `ROS_DOMAIN_ID`, `server` is driveable by
#   ros2 param get  /client_demo_server max_speed
#   ros2 param set  /client_demo_server max_speed 80
# and `client` drives any node that serves parameters (e.g. the hiroz demo server).

using ROSNode

# The schema *is* the structure: a `@parameters` struct is the shared contract the
# server bakes in and the client uses as a typed lens (just like a message/service
# type is shared across its two ends). `max_speed` mirrors the hiroz demo.
@parameters struct DemoParams
    "maximum speed"     max_speed::Int64 = 50 ∈ 0..100
    "planner mode"      mode::String = "auto" ∈ ("auto", "manual")
end

# Which half(s) to run: the `ROLE` env var or the first CLI arg, defaulting to `both`.
const ROLE = lowercase(get(ENV, "ROLE", isempty(ARGS) ? "both" : ARGS[1]))
ROLE in ("both", "server", "client") ||
    error("usage: parameters.jl [both|server|client] (got $(repr(ROLE)))")
const RUN_SERVER = ROLE in ("both", "server")
const RUN_CLIENT = ROLE in ("both", "client")

@context(peers = ["tcp/localhost:7447"]) do ctx
    # ── Server ──────────────────────────────────────────────────────────────
    # `Node(ctx, name, Schema)` attaches a typed `ParameterServer` at
    # `node.parameters` and wires the six parameter services + `/parameter_events`.
    # `overrides` overlays startup values (CLI/launch/YAML) onto the schema defaults.
    server = nothing
    if RUN_SERVER
        server = Node(ctx, "client_demo_server", DemoParams; overrides = (max_speed = 50,))
        @info "serving parameters" max_speed = server.parameters.max_speed mode = server.parameters.mode
    end

    # ── Client ──────────────────────────────────────────────────────────────
    if RUN_CLIENT
        node = Node(ctx, "client_demo")
        # Typed client: shares `DemoParams`, so `fetch`/`transaction` are type-stable.
        # Drop the schema arg (`ParameterClient(node, "/client_demo_server")`) to talk
        # to a node whose schema you don't have — then it's dynamic (`Any`/NamedTuple).
        client = ParameterClient(node, "/client_demo_server", DemoParams)

        # Block until the parameter services are routing-matched — no sleep. `false`
        # on timeout (e.g. no server up), so we don't fire calls into the void.
        if wait_for_service(client; timeout = 5)
            # ── L0: the low-level verbs, 1:1 with the hiroz client ──────────────
            @info "remote values" get_parameters(client, [:max_speed])              # → Any[50]
            @info "remote types"  get_parameter_types(client, [:max_speed])         # → [PARAMETER_INTEGER]
            ok, reason = set_parameters_atomically(client, [:max_speed => 80])
            @info "remote atomic set" ok reason

            # ── L2: the Julian structure — fetch a typed snapshot, read it locally ──
            p = fetch(client)                                                       # ::DemoParams (one round-trip)
            @info "snapshot" max_speed = p.max_speed mode = p.mode

            # Watch the remote's changes (the remote dual of on_parameter_event).
            on_parameter_event(client) do batch
                @info "remote param event" changed = batch.changed
            end

            # Mutate the remote with the same do-block as the server — pushed as one
            # atomic set; a rejected value raises ParameterRejection (uniform local↔remote).
            transaction(client) do d
                d.max_speed = 8
                d.mode      = "manual"
            end
            sleep(0.5)   # let the /parameter_events notification arrive

            try
                set_parameter!(client, :max_speed, 999)   # out of [0, 100]
            catch e
                e isa ParameterRejection ? (@warn "rejected" e.reason) : rethrow()
            end
        else
            @warn "no /client_demo_server parameter services within 5s — start one (e.g. `parameters.jl server`)"
        end
        close(client)
        close(node)
    end

    # In `both`, observe the remote sets landed on the server's typed struct.
    RUN_SERVER && RUN_CLIENT &&
        @info "after remote sets" max_speed = server.parameters.max_speed mode = server.parameters.mode

    # Server-only: keep serving until interrupted. (In `both`, the client already ran.)
    if RUN_SERVER && !RUN_CLIENT
        @info "serving parameters on /client_demo_server — Ctrl-C to stop"
        spin(ctx; handle_signals = true)
    end

    server === nothing || close(server)
end
