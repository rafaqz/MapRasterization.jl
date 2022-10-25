module MapRasterization

using Colors
using ColorVectorSpace
using Combinatorics
using GeometryBasics
using GLM
using IntervalSets
using InvertedIndices
using LinearAlgebra
using Makie
using MixedModels
using Neighborhoods
using Observables
using Rasters
using Statistics

import Tables
import ImageSegmentation as IS
import GeoInterface as GI

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
