using Documenter
using ROSNode

# Generate the interactive state-machine definitions from ROSNode's reified transition
# tables, so the diagrams embedded in the concept pages track the runtime.
include("statemachines.jl")
StateMachines.write_js(joinpath(@__DIR__, "src", "assets", "rosnode-machines.js"))

makedocs(;
    sitename = "ROSNode.jl",
    modules = [ROSNode],
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", nothing) == "true",
        assets = ["assets/xstate.umd.min.js", "assets/rosnode-machines.js",
                  "assets/statechart.js", "assets/statechart.css"]),
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
