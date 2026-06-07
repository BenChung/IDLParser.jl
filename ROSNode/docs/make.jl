using Documenter

# API autodocs can be added later with `using ROSNode` and `modules = [ROSNode]`.
# This first cut is prose-only, so the build stays independent of the workspace.
makedocs(;
    sitename = "ROSNode.jl",
    format = Documenter.HTML(; prettyurls = get(ENV, "CI", nothing) == "true"),
    checkdocs = :none,
    pages = [
        "Home" => "index.md",
        "Getting Started" => "getting-started.md",
        "Interface Types" => "interfaces.md",
        "Tutorials" => [
            "Publisher and Subscriber" => "tutorials/pubsub.md",
            "Service and Client" => "tutorials/service.md",
            "Actions" => "tutorials/action.md",
        ],
    ],
)

deploydocs(;
    repo = "github.com/BenChung/IDLParser.jl.git",
    devbranch = "main",
)
