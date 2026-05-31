module IDLParser

module Parse
    include("parsing/types.jl")
    include("parsing/parse.jl")
    include("parsing/unparse.jl")
end

module ConstResolution
    include("const_resolution/types.jl")
    include("const_resolution/const_resolve.jl")
end

module Generation
    include("generation/gen.jl")
end

# NB: no precompile workload here on purpose. ROSNode's `@ros_msgs` path uses only
# `ConstResolution.resolve_constants` + `Generation.generate_code` — never the IDL
# *text* parser (`Parse.specification`). Baking the full IDL grammar here cost ~14s
# of cold-build time compiling combinators no ROSNode consumer calls. Coverage for
# the resolve+generate methods ROSNode *does* hit is baked by ROSMessages' workload
# (it runs the real parse_msg → resolve → generate chain, caching those external
# CodeInstances into ROSMessages' pkgimage). A standalone `.idl` consumer that wants
# the parser baked should add its own workload exercising `open_idl`/`parse_whole`.

end
