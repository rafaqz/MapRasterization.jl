module RasterUtils

using Colors
using ColorVectorSpace
using GeometryBasics
using GLM
using Makie
using Observables
using Rasters
using Statistics
using Tables

using Rasters.LookupArrays

include("selectpoints.jl")
include("selectcolors.jl")
include("warp.jl")

end
