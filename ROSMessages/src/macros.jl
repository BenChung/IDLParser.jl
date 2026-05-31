_pipeline_kind_from_path(path::AbstractString) = begin
    parts = splitpath(path)
    idx = findlast(s -> s in ("msg", "srv", "action"), parts)
    idx === nothing &&
        error("@ros_msg path must contain a /msg/, /srv/, or /action/ component: $path")
    (parts[idx-1], parts[idx], splitext(parts[end])[1])
end

# Reads the file and runs it through whichever ROS2 entry point matches its
# kind, returning the parsed top-level decls.
function _parse_one(path::AbstractString)
    pkg, kind, name = _pipeline_kind_from_path(path)
    src = read(path, String)
    if kind == "msg"
        parse_msg(src; name=name, package=pkg)
    elseif kind == "srv"
        parse_srv(src; name=name, package=pkg)
    else
        # An action generates its three sections plus the implicit SendGoal /
        # GetResult / FeedbackMessage protocol types rosidl derives — both land
        # in the same `<pkg>::action` module (the generator merges them).
        decls = parse_action(src; name=name, package=pkg)
        append!(decls, action_protocol_decls(name; package=pkg))
        decls
    end
end

# Macro args come in as literal strings; this lifts them into absolute paths
# resolved against the source file containing the macro call (matching
# `include` semantics). REPL calls fall back to the current working dir.
function _resolve_macro_path(src::LineNumberNode, path)
    path isa AbstractString ||
        error("ros_msg macros take string literals, got $(typeof(path))")
    isabspath(path) && return String(path)
    base = src.file === nothing ? pwd() : dirname(String(src.file))
    return abspath(joinpath(base, path))
end

# Expand a directory into the .msg/.srv/.action files it contains.
function _expand_paths(paths::Vector{String})
    out = String[]
    for p in paths
        if isdir(p)
            for (dir, _, files) in walkdir(p)
                for f in sort!(copy(files))
                    splitext(f)[2] in (".msg", ".srv", ".action") || continue
                    push!(out, joinpath(dir, f))
                end
            end
        elseif isfile(p)
            push!(out, p)
        else
            error("path is neither file nor directory: $p")
        end
    end
    return out
end

# Run the merged parse → resolve → generate pipeline over `files` and produce
# the block to splice into the caller's module. Every file is registered with
# `Base.include_dependency` so precompilation reruns when sources change.
function _expand_msg_files(files::Vector{String})
    combined = Parse.Decl[]
    seen = Set{Tuple{String, String, String}}()
    for path in files
        pkg, kind, name = _pipeline_kind_from_path(path)
        key = (pkg, kind, name)
        key in seen && continue
        push!(seen, key)
        append!(combined, _parse_one(path))
    end
    resolved = ConstResolution.resolve_constants(combined)
    code = Generation.generate_code(resolved)

    # `Expr(:toplevel, ...)` — `module` definitions are top-level statements;
    # an `Expr(:block, ...)` would reject them as ordinary expressions.
    block = Expr(:toplevel)
    for path in files
        push!(block.args, :($Base.include_dependency($path)))
    end
    append!(block.args, code)
    return block
end

"""
    @ros_msg "path/to/Foo.msg"

Parse a single ROS2 interface file (`.msg`, `.srv`, or `.action`) and splice
the generated Julia types into the current module at macro-expansion time.
The package is inferred from the path layout — the directory containing
`msg`/`srv`/`action` is the package.

Relative paths resolve against the source file containing the macro call.
The file is registered as a precompile dependency.
"""
macro ros_msg(path)
    # `esc` so the generated `module` blocks land in the caller's namespace
    # verbatim, without Julia's hygiene wrapping the `module` head.
    return esc(_expand_msg_files([_resolve_macro_path(__source__, path)]))
end

"""
    @ros_msgs "path1" "path2" ...

Like [`@ros_msg`](@ref) but accepts any number of file or directory paths.
Directories are walked recursively for `.msg`/`.srv`/`.action` files. All
collected files are run through the pipeline together so cross-package
references resolve in one shot.
"""
macro ros_msgs(paths...)
    resolved = [_resolve_macro_path(__source__, p) for p in paths]
    return esc(_expand_msg_files(_expand_paths(resolved)))
end
