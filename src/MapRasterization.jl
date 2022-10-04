module MapRasterization

using Colors
using ColorVectorSpace
using Combinatorics
using DynamicGrids
using GeometryBasics
using GLM
using InvertedIndices
using Makie
using MixedModels
using Observables
using LinearAlgebra
using Rasters
using Statistics

import Tables
import ImageSegmentation as IS
import GeoInterface as GI

using DynamicGrids.Neighborhoods
using Rasters.LookupArrays

include("balance.jl")
include("scan.jl")
include("categories.jl")
include("clean.jl")
include("stripes.jl")
include("selectpoints.jl")
include("selectcolors.jl")
include("warp.jl")
include("blur.jl")

end
