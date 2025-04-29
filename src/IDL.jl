module IDL

module Parse
    include("parsing/types.jl")
    include("parsing/parse.jl")
end

module ConstResolution
    include("const_resolution/types.jl")
    include("const_resolution/const_resolve.jl") 
end

module Generation
    include("generation/gen.jl")
end

end
