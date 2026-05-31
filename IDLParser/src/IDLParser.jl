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

# Bake the parse → resolve → generate pipeline into the pkgimage. The PEG parser
# combinators and the Moshi-`@match` walkers cost several seconds to JIT on first
# use; without this every downstream consumer (ROSMessages' `@ros_msgs`, …) pays
# that at *its* precompile. The snippet covers the constructs that drive the
# expensive specializations: modules, a const expr, a struct with scalars, a
# bounded string, fixed array, bounded/unbounded sequences, a typedef, a ref.
using PrecompileTools: @setup_workload, @compile_workload
using PEG: parse_whole
@setup_workload begin
    idl = """
    module pkg {
      module msg {
        const long FOO = 1 + 2 * 3;
        typedef double Vec3[3];
        struct Sample {
          long a;
          double b;
          boolean flag;
          string<16> label;
          double arr[4];
          Vec3 v;
          sequence<long> seq;
          sequence<double, 8> bseq;
        };
      };
    };
    """
    @compile_workload begin
        parsed = parse_whole(Parse.specification, idl)
        resolved = ConstResolution.resolve_constants(
            convert(Vector{Parse.Decl}, parsed))
        Generation.generate_code(resolved)
    end
end

end
