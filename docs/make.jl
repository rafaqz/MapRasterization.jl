using MapRasterization
using Documenter

DocMeta.setdocmeta!(MapRasterization, :DocTestSetup, :(using MapRasterization); recursive=true)

makedocs(;
    modules=[MapRasterization],
    authors="Rafael Schouten <rafaelschouten@gmail.com>",
    repo="https://github.com/rafaqz/MapRasterization.jl/blob/{commit}{path}#{line}",
    sitename="MapRasterization.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://rafaqz.github.io/MapRasterization.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/rafaqz/MapRasterization.jl",
    devbranch="main",
)
