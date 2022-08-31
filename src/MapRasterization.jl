module MapRasterization

using Colors
using ColorVectorSpace
using GeometryBasics
using GLM
using Makie
using Observables
using ImageSegmentation
using Rasters
using Statistics
using Tables

using Rasters.LookupArrays

include("selectpoints.jl")
include("selectcolors.jl")
include("warp.jl")

end