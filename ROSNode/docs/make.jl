using Documenter
using ROSNode

makedocs(;
    sitename = "ROSNode.jl",
    modules = [ROSNode],
    format = Documenter.HTML(; prettyurls = get(ENV, "CI", nothing) == "true"),
    checkdocs = :exports,
    pages = [
        "Home" => "index.md",
        "Getting Started" => "getting-started.md",
        "Foundations" => [
            "The Runtime Model" => "foundations/runtime-model.md",
            "Interface Types" => "foundations/interface-types.md",
        ],
        "Communication" => [
            "Topics" => "communication/topics.md",
            "Services" => "communication/services.md",
            "Actions" => "communication/actions.md",
            "Parameters" => "communication/parameters.md",
        ],
        "Going Further" => [
            "Authoring Interfaces in Julia" => "advanced/authoring.md",
            "Runtime Type Discovery" => "advanced/discovery.md",
            "Message Delivery" => "advanced/delivery.md",
        ],
        "Composition" => [
            "Components" => "composition/components.md",
        ],
        "Interoperating with ROS 2" => "interop/ros2.md",
    ],
)

deploydocs(;
    repo = "github.com/BenChung/IDLParser.jl.git",
    devbranch = "main",
)
