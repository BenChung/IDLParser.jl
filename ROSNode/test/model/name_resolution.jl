# §5 name resolution. `resolve_name` is duck-typed against a node's
# `namespace`/`name`/`fqn`/`remaps`, so we exercise it with lightweight mock
# nodes — no Context/session. Covers absolute / relative / private forms, FQN
# validation, the remap table, and the bare-Context (no node FQN) fallbacks.

using ROSNode: resolve_name

# A node stand-in carrying exactly the fields the resolver reads.
struct ResolveMockNode
    namespace::String
    name::String
    fqn::String
    remaps::Vector{Pair{String, String}}
end
ResolveMockNode(ns, name; remaps=Pair{String,String}[]) =
    ResolveMockNode(ns, name, rstrip(ns == "/" ? "/$name" : "$ns/$name", '/'), remaps)

# A bare-Context stand-in: only a default namespace, no node FQN (so `~` errors).
struct ResolveMockCtx
    namespace::String
end

@testset "name resolution" begin
    node = ResolveMockNode("/robot", "planner")

    @testset "absolute names pass through" begin
        @test resolve_name(node, "/foo") == "/foo"
        @test resolve_name(node, "/foo/bar") == "/foo/bar"
        @test resolve_name(node, "/") == "/"
    end

    @testset "relative names prepend the namespace" begin
        @test resolve_name(node, "foo") == "/robot/foo"
        @test resolve_name(node, "foo/bar") == "/robot/foo/bar"
        # root-namespace node: relative joins without doubling the slash
        root = ResolveMockNode("/", "n")
        @test resolve_name(root, "chatter") == "/chatter"
    end

    @testset "private names prepend the node FQN" begin
        @test resolve_name(node, "~/cfg") == "/robot/planner/cfg"
        @test resolve_name(node, "~") == "/robot/planner"      # bare ~ is the FQN
        @test resolve_name(node, "~/a/b") == "/robot/planner/a/b"
    end

    @testset "private names need a node FQN (bare Context errors)" begin
        ctx = ResolveMockCtx("/ns")
        # absolute + relative still resolve against the default namespace
        @test resolve_name(ctx, "/abs") == "/abs"
        @test resolve_name(ctx, "rel") == "/ns/rel"
        # private has no node FQN ⇒ ArgumentError
        @test_throws ArgumentError resolve_name(ctx, "~/x")
    end

    @testset "remap rules apply after resolution (exact FQN match)" begin
        remapped = ResolveMockNode("/robot", "planner";
                                   remaps=["/robot/foo" => "/robot/bar"])
        @test resolve_name(remapped, "foo") == "/robot/bar"    # relative → remapped FQN
        @test resolve_name(remapped, "/robot/foo") == "/robot/bar"
        @test resolve_name(remapped, "baz") == "/robot/baz"    # unmatched untouched
    end

    @testset "validation rejects malformed resolved names" begin
        @test_throws ArgumentError resolve_name(node, "")             # empty
        # a remap producing a structurally-invalid name is caught by validation
        bad = ResolveMockNode("/robot", "planner"; remaps=["/robot/x" => "no_leading_slash"])
        @test_throws ArgumentError resolve_name(bad, "x")
        dbl = ResolveMockNode("/robot", "planner"; remaps=["/robot/x" => "/a//b"])
        @test_throws ArgumentError resolve_name(dbl, "x")
        trail = ResolveMockNode("/robot", "planner"; remaps=["/robot/x" => "/a/b/"])
        @test_throws ArgumentError resolve_name(trail, "x")
        digit = ResolveMockNode("/robot", "planner"; remaps=["/robot/x" => "/9bad"])
        @test_throws ArgumentError resolve_name(digit, "x")
    end

    @testset "namespace normalization (leading slash, no trailing)" begin
        # constructed with a trailing-slash namespace; relative joins are clean
        weird = ResolveMockNode("/a/b/", "n")
        @test resolve_name(weird, "t") == "/a/b/t"
        # a namespace without a leading slash is normalized for relative joins
        noslash = ResolveMockNode("a", "n")
        @test resolve_name(noslash, "t") == "/a/t"
    end

    @testset "valid name tokens (letters, digits, underscore; not leading digit)" begin
        @test resolve_name(node, "/topic_1") == "/topic_1"
        @test resolve_name(node, "/_hidden") == "/_hidden"
        @test_throws ArgumentError resolve_name(node, "/1bad")
        @test_throws ArgumentError resolve_name(node, "/has-dash")
    end
end
