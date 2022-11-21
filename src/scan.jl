const DEFAULT_MATCH = Val{(:color,:std,:stripe)}()

struct PixelProp{C}
    color::C
    std::Float64
    stripe::Tuple{Float64,Float64}
    category::Int
end

import Base: +, -, /, *
(+)(a::PixelProp, b::PixelProp) = PixelProp(a.color + b.color, a.std + b.std, a.stripe .+ b.stripe, _mergecategory(a.category, b.category))
(-)(a::PixelProp, b::PixelProp) = PixelProp(a.color - b.color, a.std - b.std, a.stripe .- b.stripe, _mergecategory(a.category, b.category))
(/)(a::PixelProp, b::Real) = PixelProp(a.color / b, a.std / b, a.stripe ./ b, a.category)
(*)(a::PixelProp, b::Real) = PixelProp(a.color * b, a.std * b, a.stripe .* b, a.category)

category(p::PixelProp) = p.category
category(p) = 0
function _mergecategory(a, b)
    if a == 0 
        return b
    elseif b == 0 
        return a
    elseif a == b 
        return a
    else
        error("cannot add pixels of different categories: $a and $b")
    end
end

Base.zero(::Type{<:PixelProp{C}}) where {C} =
    PixelProp(zero(C), 0.0, (0.0, 0.0), 0) 
Base.oneunit(::Type{<:PixelProp{C}}) where {C} = PixelProp(oneunit(C), 1.0, (1.0, 1.0), 0)
Base.one(::Type{<:PixelProp{C}}) where {C} = PixelProp(one(C), 1.0, (1.0, 1.0), 0)
Base.convert(::Type{<:PixelProp{C}}, p::PixelProp) where {C} =
    PixelProp(C(p.color), p.std, (p.stripe[1], p.stripe[2]), p.category)
Base.convert(::Type{<:PixelProp{C}}, p::PixelProp) where {C} = PixelProp(C(p.color), p.std, p.stripe, p.category) 
Base.convert(::Type{T}, p::PixelProp) where T = Base.convert(T, p.color)

IS.accum_type(::Type{P}) where P <: PixelProp{C} where {C} = PixelProp{IS.accum_type(C)}

meantype(::Type{T}) where T = typeof(zero(IS.accum_type(T))/2)

_combine(arrays::AbstractArray...) = map((c, sd, sp) -> PixelProp(c, sd, sp), arrays...)

_abs2(c::IS.MathTypes) = mapreducec(v -> float(v)^2, +, 0, c)
_abs2(x) = abs2(x)

getscalar(A::AbstractArray{T,N}, i::CartesianIndex{N}, block_length::CartesianIndex{N}) where {T<:Real,N} =
    A[CartesianIndex(ntuple(j->(i[j]-1)÷block_length[j]+1, Val(N)))]

getscalar(a::Real, i...) = a

fast_scanning(img::AbstractArray, threshold::Real; kw...) =
    fast_scanning!(fill(-1, axes(img)), img, threshold; kw...)

function fast_scanning!(result, img::AbstractArray{CT,N}, threshold::Union{AbstractArray,Real};
    match::Val=DEFAULT_MATCH,
) where {CT,N,DF<:Function}

    # Neighbourhood function
    _diagmN = Diagonal([1 for i in 1:N])
    half_region::NTuple{N,CartesianIndex{N}} = ntuple(i-> CartesianIndex{N}(ntuple(j->_diagmN[j,i], Val(N))), Val(N))
    neighbourhood(x) = ntuple(i-> x-half_region[i], Val(N))

    # Required data structures
    TM = meantype(CT)
    region_means        =   Dict{Int, TM}()                                 # A map conatining (label, mean) pairs
    region_pix_count    =   Dict{Int, Int}()                                # A map conatining (label, count) pairs
    region_bbox         =   Dict{Int, Tuple{CartesianIndex,CartesianIndex}}() # 
    temp_labels         =   IS.IntDisjointSets(0)                           # Disjoint set to map labels to their equivalence class
    v_neigh             =   IS.MVector{N,Int}(undef)                        # MVector to store valid neighbours
    ctg_neigh           =   IS.MVector{N,Int}(undef)                        # MVector to store valid neighbours

    block_length = CartesianIndex(ntuple(i->ceil(Int,length(axes(img,i))/size(threshold,i)), Val(N)))

    for point in CartesianIndices(axes(img))
        ctg = category(img[point])
        sz = 0
        same_label = true
        prev_label = 0
        for p in neighbourhood(point)
            if checkbounds(Bool, img, p)
                root_p = IS.find_root!(temp_labels, result[p])
                if _category_error(region_means[root_p], img[point], match) < IS.getscalar(threshold, point, block_length)
                    if prev_label == 0
                        prev_label = root_p
                    elseif prev_label != root_p
                        same_label = false
                    end
                    sz += 1
                    v_neigh[sz] = IS.find_root!(temp_labels, root_p)
                    ctg_neigh[sz] = region_means[v_neigh[sz]].category
                end
            end
        end

        # If no valid label found
        if sz == 0
            # Assign a new label
            new_label = push!(temp_labels)
            result[point] = new_label
            region_means[new_label] = img[point]
            region_pix_count[new_label] = 1
            region_bbox[new_label] = (point, point)

        # If all labels are same
        elseif same_label && _canmerge(ctg, region_means[prev_label].category)
            result[point] = prev_label
            region_pix_count[prev_label] += 1
            region_means[prev_label] += (img[point] - region_means[prev_label])/(region_pix_count[prev_label])
            region_bbox[prev_label] = _merge_bbox(region_bbox[prev_label], point)
        # All the same category or uncategorized
        elseif all(map(i -> _canmerge(ctg, ctg_neigh[i]), 1:sz)) && (sz < 2 || _canmerge(ctg_neigh[1], ctg_neigh[2]))
            # Merge segments and assign to this new label
            union_label = v_neigh[1]
            for i in 1:sz
                union_label = union!(temp_labels, union_label, v_neigh[i])
            end
            result[point] = union_label
            region_pix_count[union_label] += 1
            region_means[union_label] += (img[point] - region_means[union_label])/(region_pix_count[union_label])
            region_bbox[union_label] = _merge_bbox(region_bbox[union_label], point)

            for i in 1:sz
                if v_neigh[i] != union_label && haskey(region_pix_count, v_neigh[i])
                    region_pix_count[union_label] += region_pix_count[v_neigh[i]]
                    region_means[union_label] += (region_means[v_neigh[i]] - region_means[union_label])*region_pix_count[v_neigh[i]]/region_pix_count[union_label]
                    region_bbox[union_label] = _merge_bbox(region_bbox[v_neigh[i]], region_bbox[union_label])

                    # Remove label v_neigh[i] from region_means, region_pix_count
                    delete!(region_pix_count,v_neigh[i])
                    delete!(region_means,v_neigh[i])
                    delete!(region_bbox,v_neigh[i])
                end
            end
        # elseif ctg == 0 
        #     # Assign to closest
        #     new_label = push!(temp_labels)
        #     result[point] = new_label
        #     region_means[new_label] = img[point]
        #     region_pix_count[new_label] = 1
        else
            # Assign a new label
            new_label = push!(temp_labels)
            result[point] = new_label
            region_means[new_label] = img[point]
            region_pix_count[new_label] = 1
            region_bbox[new_label] = (point, point)
        end
    end

    for point in CartesianIndices(axes(img))
        result[point] = IS.find_root!(temp_labels, result[point])
    end

    IS.SegmentedImage(result, unique(temp_labels.parents), region_means, region_pix_count, region_bbox)
end

function _merge_bbox(a::Tuple{CartesianIndex,CartesianIndex}, b::Tuple{CartesianIndex,CartesianIndex})
    min(a..., b...), max(a..., b...)
end

function _merge_bbox(a::Tuple{CartesianIndex,CartesianIndex}, b::CartesianIndex)
    min(a..., b), max(a..., b)
end

_canmerge(a, b) = (a == 0) | (b == 0) | (a == b)
