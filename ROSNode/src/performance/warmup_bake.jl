# Deferred package warm-up bake — the `@setup_workload`/`@compile_workload` extracted from
# warmup.jl so it runs AFTER its late dependencies are defined. `@compile_workload` executes at
# this file's include point, and the bake reaches functions defined late in include order:
# `wire_get_type_description!` (introspection.jl), `_package_iface_files` via `_wellknown_entries`
# (staticgen.jl), and `unregister_local_subscription!` through the inert-session `close` path
# (intraprocess.jl). Run from warmup.jl's own early include point (needed there for `WarmupMode`,
# which lifecycle.jl references), the bake fired before those were defined → caught `UndefVarError`s
# that silently skipped the bring-up bake. So warmup.jl keeps the types/functions and this file —
# included last in ROSNode.jl — carries only the bake.
# ── package warm-up (PrecompileTools) ──────────────────────────────────────────
# Bakes the codec round-trip into ROSNode's pkgimage over two representative vendored
# types — one all-fixed (`@cdr_fixed` path), one with string fields (`@kwdef`/
# `read_view` path). Transport-free, so it is precompile-safe; through-the-transport
# dispatch warming stays per-entity at construction where a live session exists.
@setup_workload begin
    @compile_workload begin
        for T in (Interfaces.builtin_interfaces.msg.Time,        # all-fixed
                  Interfaces.type_description_interfaces.msg.KeyValue)  # strings
            msg = _default_msg(T)
            z = encode(msg)
            decode_owned(as_memory(z, UInt8), T)
            decode_view(as_memory(z, UInt8), T)
            # the live receive path decodes a view over a `Zenoh.PayloadView`, not a `Memory`;
            # anchor that specialization (can't synthesize a borrowed payload here, so precompile it).
            precompile(decode_view, (Zenoh.PayloadView, T))
        end
        # The six standard parameter services + /parameter_events ride every node that
        # calls `wire_parameter_services!`, and their request/response codecs are over
        # fixed `rcl_interfaces` types — so bake that shared codec layer once here rather
        # than re-JITing it at the first node with parameters. The per-mixin `ParameterServer{P_M}`
        # service *construction* is schema-specific and stays `@precompile_nodes`' job.
        let RCL = Interfaces.rcl_interfaces.srv
            for (ReqT, RespT) in ((RCL.DescribeParameters_Request,        RCL.DescribeParameters_Response),
                                  (RCL.GetParameterTypes_Request,         RCL.GetParameterTypes_Response),
                                  (RCL.GetParameters_Request,             RCL.GetParameters_Response),
                                  (RCL.ListParameters_Request,            RCL.ListParameters_Response),
                                  (RCL.SetParameters_Request,             RCL.SetParameters_Response),
                                  (RCL.SetParametersAtomically_Request,   RCL.SetParametersAtomically_Response))
                precompile(decode_owned, (Memory{UInt8}, Type{ReqT}))          # view=false: copied Memory
                precompile(decode_view,  (Zenoh.PayloadView, Type{ReqT}))      # view=true: borrowed PayloadView
                precompile(encode,       (RespT,))
                precompile(service_type_info_of, (Type{ReqT}, Type{RespT}))    # service-level type identity
            end
            precompile(encode, (Interfaces.rcl_interfaces.msg.ParameterEvent,))
        end
        # Every node hosts `~/get_type_description` (dynamic type discovery, `serve_type_description`),
        # so its codec + service-type identity over the fixed `type_description_interfaces` types bake
        # once here rather than JITing at the first node's bring-up.
        let TD = Interfaces.type_description_interfaces.srv
            precompile(decode_owned, (Memory{UInt8}, Type{TD.GetTypeDescription_Request}))
            precompile(decode_view,  (Zenoh.PayloadView, Type{TD.GetTypeDescription_Request}))
            precompile(encode, (TD.GetTypeDescription_Response,))
            precompile(service_type_info_of, (Type{TD.GetTypeDescription_Request}, Type{TD.GetTypeDescription_Response}))
            # the consumer-task setup (its handler reaches `_spawn_service_consumer` as an abstract
            # `Function`, view=false ⇒ Bool, Serial concurrency)
            precompile(_spawn_service_consumer, (Entity, Type{TD.GetTypeDescription_Request},
                                                 Type{TD.GetTypeDescription_Response}, Function, Bool, Serial))
        end
        # Each of the seven standard services above spawns a consumer task whose body is
        # `while true; serve(take!(qable)); end` → `_serve_query(…, handler, view)`. When that
        # task is first scheduled at a node's bring-up, inferring the loop drags in the WHOLE
        # serve tree (request decode → handler → `ResultCell`/`ScopedValues` settle → response
        # encode) — the single largest first-`run` framework cost. The codec anchors above don't reach it: the serve
        # tree keys on the *concrete* handler type and the owned-query type, which those lines
        # never name. Anchor the concrete `_serve_query` per service — inference dominates, and
        # `precompile` caches inference — so it bakes here once for every node rather than
        # re-inferring at the first node's bring-up.
        let RCL = Interfaces.rcl_interfaces.srv,
            TD = Interfaces.type_description_interfaces.srv,
            Q  = Zenoh.Query{Base.RefValue{Zenoh.LibZenohC.z_owned_query_t}}
            for (Op, ReqT, RespT) in ((:describe,   RCL.DescribeParameters_Request,      RCL.DescribeParameters_Response),
                                      (:get_types,  RCL.GetParameterTypes_Request,       RCL.GetParameterTypes_Response),
                                      (:get,        RCL.GetParameters_Request,           RCL.GetParameters_Response),
                                      (:list,       RCL.ListParameters_Request,          RCL.ListParameters_Response),
                                      (:set,        RCL.SetParameters_Request,           RCL.SetParameters_Response),
                                      (:set_atomic, RCL.SetParametersAtomically_Request, RCL.SetParametersAtomically_Response))
                # param services hold a concrete `_ParamSvcHandler{Op, CompositeParameterServer}`
                H = _ParamSvcHandler{Op, CompositeParameterServer}
                precompile(_serve_query, (Entity, Q, Type{ReqT}, Type{RespT}, H, Bool))
                # The consumer task body (`_service_consume_loop`, now named): anchor it on the
                # concrete Serial scheduler closure so each service's first schedule doesn't
                # re-infer the loop + node-logger scope. The scheduler closure type isn't
                # nameable, so derive it with `return_types`.
                Ss = Base.return_types(_service_scheduler, (Serial, Entity, Type{ReqT}, Type{RespT}, H, Bool))
                if length(Ss) == 1 && isconcretetype(only(Ss))
                    precompile(_service_scheduler, (Serial, Entity, Type{ReqT}, Type{RespT}, H, Bool))
                    precompile(_service_consume_loop, (only(Ss), Zenoh.QueryableHandler, Entity))
                end
            end
            # get_type_description's handler is held abstractly (`Function`) at runtime
            precompile(_serve_query, (Entity, Q, Type{TD.GetTypeDescription_Request},
                                      Type{TD.GetTypeDescription_Response}, Function, Bool))
        end
        # /rosout: the node logger encodes an `rcl_interfaces.msg.Log` on the first record, which
        # fires during bring-up. Fixed type → bake the encode here (the lazy `_rosout_publisher!`
        # builder is anchored in introspection.jl, where it is defined).
        precompile(encode, (Interfaces.rcl_interfaces.msg.Log,))
        # Discovery: every endpoint a node declares loops back through the liveliness consumer,
        # which parses the token + QoS off the fixed `RmwZenoh` format. Anchoring `parse_liveliness`
        # bakes that parse subtree (it calls `decode_qos` internally); `parse_topic_keyexpr` is the
        # service/topic-name parse the graph introspection runs.
        precompile(parse_liveliness,    (RmwZenoh, String))
        precompile(parse_topic_keyexpr, (RmwZenoh, String))
        # Zenoh's own @compile_workload bakes the generic entity-declare path, but the
        # kwarg-sorter body / option-builder / declare-closure frames specialize on the EXACT
        # kwarg-NamedTuple shape, and ROSNode declares through shapes the generic workload never
        # names. Bake them here by replaying ROSNode's actual declare call expressions against a
        # no-connect session (multicast off + no connect endpoint ⇒ ~0.3 ms open, router-free,
        # deterministic). The `_advanced_*_kwargs(qos)...` splat is load-bearing: it lowers to a
        # `#…#kwsorter(::Base.Pairs)` frame distinct from a direct literal-kwarg `Core.kwcall`, so
        # the call must be byte-for-byte `declare_publisher!`/`declare_subscription!`'s, both QoS
        # variants (volatile ⇒ plain, transient_local ⇒ Advanced). Teardown is synchronous: `close`
        # defers a buffered sub's teardown to a `@spawn`'d finalizer precompilation won't run, so
        # drive `_teardown_buffered_sub!` directly to leave no AsyncCondition open. Best-effort —
        # a sandbox that can't open a session still builds.
        try
            # Timestamping on (matches Zenoh's own proven workload config): the AdvancedPublisher
            # declare needs it, and a no-connect peer never reaches the wire so it costs nothing.
            s  = Base.open(Config(; str =
                "{mode:\"peer\",scouting:{multicast:{enabled:false}},timestamping:{enabled:true}}"))
            ke = Keyexpr("rosnode/_precompile")
            # The every-startup declares first and unconditionally — `declare_publisher!`'s plain
            # publisher and `declare_subscription!`'s buffered (`:fifo`) open, both at ROSNode's
            # exact volatile-QoS kwarg shape (the `_advanced_*_kwargs(qos)...` splat is the live one).
            pub = ZPublisher(s, ke; reliability = Reliabilities.RELIABLE,
                             congestion_control = nothing, priority = nothing,
                             _advanced_pub_kwargs(default_qos())...)
            close(pub)
            sub = Base.open(s, ke; channel = :fifo, capacity = _fifo_capacity(default_qos()),
                            allowed_origin = Zenoh.Localities.ANY, _advanced_sub_kwargs(default_qos())...)
            close(sub)
            Zenoh._teardown_buffered_sub!(sub)
            # context.jl `_start_discovery!`: the keepall (`history=true`) liveliness subscriber.
            lv = LivelinessSubscriberHandler(s, ke; channel = :fifo, capacity = 1024, history = true)
            close(lv)
            Zenoh._teardown_buffered_sub!(lv)
            # transient_local ⇒ AdvancedPublisher: rarer opt-in, and its declare is the one that can
            # fail in a constrained sandbox — isolate it so a throw can't skip the declares above.
            try
                apub = ZPublisher(s, ke; reliability = Reliabilities.RELIABLE,
                                  congestion_control = nothing, priority = nothing,
                                  _advanced_pub_kwargs(QosProfile(durability = :transient_local))...)
                close(apub)
            catch
            end
            close(s)
        catch
        end
        # Bring-up bake (the "inert session"): `Context`/`Node`/`make_entity`/discovery are ROSNode
        # ops on ROSNode/Zenoh types — consumer-AGNOSTIC (`make_entity` takes `Union{TypeInfo,
        # Nothing}`, NOT the message type), so they ride ROSNode's OWN image and cover every node;
        # a consumer's `@precompile_nodes` can't cache them (it caches only consumer-owned-type MIs).
        # This dynamic bring-up resists frame-by-frame `precompile` (the LAUNCH lesson — it needs
        # EXECUTION), so run a probe node on an inert no-connect session and let inference cache the
        # path (Context ctor + `_register_*` + Node + `~/get_type_description` + `make_entity` per
        # kind + the liveliness consumer). Synchronous `close` drain. Best-effort — a sandbox that
        # can't open a session still builds. (The per-message-type `_make_publisher`/`_make_service`
        # construction stays the consumer's `@precompile_nodes` job — those ARE consumer-typed.)
        try
            # Open the probe Context the SAME way `run` does (`config=nothing` + `localhost_only`,
            # peers a `Vector{String}`), so this execution bakes the EXACT runtime `Context` MI —
            # incl. the default `Config()` + `_apply_ros_transport_config!` building that the
            # config-given path skips. `localhost_only` keeps it inert (multicast scouting off).
            pctx = Context(; peers = String[], localhost_only = true)
            pnode = Node(pctx, "_precompile_probe")
            ti = type_info_of(Interfaces.builtin_interfaces.msg.Time)
            for k in (Publisher, Subscription, Service)
                make_entity(pnode, k, "rosnode/_probe", ti)
            end
            close(pctx)
        catch
        end
        # The composed-`@node` façade wiring is non-parametric — `CompositeParameterServer`
        # holds its members in a runtime field, not a type parameter — so its construction +
        # six-service wiring + `/parameter_events` aggregation is one specialisation shared by
        # every composed node. Bake it here rather than re-JITing the whole wiring at the first
        # composed node's `run` (the per-mixin `ParameterServer{P}` form stays per-schema).
        precompile(CompositeParameterServer, (Node, Vector{Pair{Symbol, ParameterServer}}))
        precompile(wire_parameter_services!, (CompositeParameterServer,))
        precompile(_wire_composite_events!,  (CompositeParameterServer,))
        # The action server `encode`s a `GoalStatusArray` onto `~/_action/status` on the first
        # goal accept and each transition. Fixed `action_msgs` type, so bake the status-snapshot
        # encode (transitively its nested `GoalStatus`/`GoalInfo`) once here instead of per node.
        precompile(encode, (Interfaces.action_msgs.msg.GoalStatusArray,))
        # Re-bake the PEG grammar combinators with Zenoh loaded. ROSMessages bakes them
        # in its Zenoh-free pkgimage, but Zenoh's `pointer(::GuardedPayloadView)`
        # specialization (GuardedPayloadView <: DenseVector) invalidates them at load,
        # so the first `Context()` re-JITs them (~4s) — the root cause of its cost.
        # Running `_wellknown_entries()` here (Zenoh present) pins valid copies in
        # ROSNode's image; it parses the same grammar the broader `_canonical_entries()`
        # uses, which can't run until staticgen.jl is included.
        _wellknown_entries()
    end
end
