# Drift guard for the tier-2 scaffolding precompile anchors (G4). Every entry in
# `_component_precompile_specs()` must resolve to a real method, so `precompile(f, ts)`
# returns `true`. A signature change that silently de-anchors a path (e.g. a renamed
# internal or a changed arity) flips one to `false` and fails here — otherwise the
# `@compile_workload` would quietly stop baking that path and read green.
#
# `Core.kwcall` anchors need a second, stronger check: the kwsorter dispatches on the target
# function + the positional types, NOT on the keyword NamedTuple's field names, so
# `precompile(Core.kwcall, …)` returns `true` for ANY keyword name — including one the target
# doesn't accept (which then bakes only the kwsorter MI, never the `#f#` body, and throws
# `unsupported keyword` at runtime). So assert each anchor's keyword names against the target's
# declared keywords (`Base.kwarg_decl`).
using ROSNode
using Test

# A `Core.kwcall` anchor is `(Core.kwcall, (NT_type, typeof(target), positionals...))`. Returns
# `(target_function, keyword_names)`, or `nothing` for a non-kwcall anchor.
function _kwanchor(f, ts)
    f === Core.kwcall || return nothing
    (target_function = ts[2].instance, keyword_names = fieldnames(ts[1]))
end

# Every keyword the target declares across its methods.
_declared_kwargs(f) = reduce(union, (Base.kwarg_decl(m) for m in methods(f)); init = Symbol[])

@testset "tier-2 scaffolding precompile anchors resolve" begin
    specs = ROSNode._component_precompile_specs()
    @test !isempty(specs)
    for (f, ts) in specs
        @test precompile(f, ts)
        kw = _kwanchor(f, ts)
        kw === nothing && continue
        declared = _declared_kwargs(kw.target_function)
        for name in kw.keyword_names
            @test name in declared      # the target actually accepts this keyword
        end
    end
end
