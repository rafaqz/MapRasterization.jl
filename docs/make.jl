using RasterUtils
using Documenter

DocMeta.setdocmeta!(RasterUtils, :DocTestSetup, :(using RasterUtils); recursive=true)

makedocs(;
    modules=[RasterUtils],
    authors="Rafael Schouten <rafaelschouten@gmail.com>",
    repo="https://github.com/rafaqz/RasterUtils.jl/blob/{commit}{path}#{line}",
    sitename="RasterUtils.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://rafaqz.github.io/RasterUtils.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/rafaqz/RasterUtils.jl",
    devbranch="main",
)
