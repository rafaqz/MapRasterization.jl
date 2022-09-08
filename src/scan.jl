
struct PixelProp{C<:Colorant,SD<:Real,SP<:Tuple{Real,Real}}
    color::C
    std::SD
    stripe::SP
    category::Int
end

import Base: +, -, /, *
(+)(a::PixelProp, b::PixelProp) = PixelProp(a.color + b.color, a.std + b.std, a.stripe .+ b.stripe, _mergecategory(a.category, b.category))
# (-)(a::PixelProp, b::PixelProp) = PixelProp(a.color - b.color, a.std - b.std, a.stripe .- b.stripe)
(/)(a::PixelProp, b::Real) = PixelProp(a.color / b, a.std / b, a.stripe ./ b, a.category)
(*)(a::PixelProp, b::Real) = PixelProp(a.color * b, a.std * b, a.stripe .* b, a.category)

category(p::PixelProp) = p.category
category(p) = 0
function _mergecategory(a, b)
    a == 0 && return b
    b == 0 && return a
    a == b && return a
    error("cannot add pixels of different categories")
end

Base.zero(::Type{<:PixelProp{C,SD,Tuple{SP,SP}}}) where {C,SD,SP} =
    PixelProp(zero(C), zero(SD), (zero(SP), zero(SP))) 
Base.oneunit(::Type{<:PixelProp{C,SD,Tuple{SP,SP}}}) where {C,SD,SP} =
    PixelProp(oneunit(C), oneunit(SD), (oneunit(SP), oneunit(SP))) 
Base.one(::Type{<:PixelProp{C,SD,Tuple{SP,SP}}}) where {C,SD,SP} =
    PixelProp(one(C), one(SD), (one(SP), one(SP))) 
Base.convert(::Type{<:PixelProp{C,SD,Tuple{SP,SP}}}, p::PixelProp) where {C,SD,SP} =
    PixelProp(C(p.color), SD(p.std), (SP(p.stripe[1]), SP(p.stripe[2]))) 
Base.convert(::Type{<:PixelProp{C}}, p::PixelProp) where {C,SD,SP} = PixelProp(C(p.color), p.std, p.stripe) 
Base.convert(::Type{C}, p::PixelProp) where {C<:Colors.Colorant} = Base.convert(C, p.color)

accum_type(::Type{T}) where {T<:Integer}    = Int
accum_type(::Type{Float32})                 = Float32
accum_type(::Type{T}) where {T<:Real}       = Float64
accum_type(::Type{T}) where {T<:Colors.FixedPoint} = Colors.floattype(T)
accum_type(::Type{C}) where {C<:Colors.Colorant}   = base_colorant_type(C){accum_type(eltype(C))}
accum_type(::Type{P}) where P <: PixelProp{C,SD,SP} where {C,SD,SP} = PixelProp{accum_type(C),SD,SP}

accum_type(val) = isa(val, Type) ? throw_accum_type(val) : convert(accum_type(typeof(val)), val)
throw_accum_type(T) = error("type $T not supported in `accum_type`")

meantype(::Type{T}) where T = typeof(zero(accum_type(T))/2)

_combine(arrays::AbstractArray...) = map((c, sd, sp) -> PixelProp(c, sd, sp), arrays...)

# default_diff_fn(c1::CT1, c2::CT2) where {CT1<:Union{Colorant,Real}, CT2<:Union{Colorant,Real}} = sqrt(_abs2(c1-accum_type(c2)))
function default_diff_fn(t1::PixelProp, t2::PixelProp)
    sqrt(
        # _abs2(t1.color - t2.color) + 
        # _abs2(t1.std - t2.std) * 5 +
        _abs2(t1.stripe[1] - t2.stripe[1]) * 5 + 
        _abs2(t1.stripe[2] - t2.stripe[2]) * 5
    )
end
default_diff_fn(x1::Real, x2::Real) = abs2(x1 - x2)

_abs2(c::IS.MathTypes) = mapreducec(v -> float(v)^2, +, 0, c)
_abs2(x) = abs2(x)

getscalar(A::AbstractArray{T,N}, i::CartesianIndex{N}, block_length::CartesianIndex{N}) where {T<:Real,N} =
    A[CartesianIndex(ntuple(j->(i[j]-1)÷block_length[j]+1, Val(N)))]

getscalar(a::Real, i...) = a

fast_scanning(img::AbstractArray, threshold::Real, diff_fn::Function = default_diff_fn) =
    fast_scanning!(fill(-1, axes(img)), img, threshold, diff_fn)

function fast_scanning!(result, img::AbstractArray{CT,N}, threshold::Union{AbstractArray,Real}, diff_fn::DF = default_diff_fn) where {CT,N,DF<:Function}

    # Neighbourhood function
    _diagmN = Diagonal([1 for i in 1:N])
    half_region::NTuple{N,CartesianIndex{N}} = ntuple(i-> CartesianIndex{N}(ntuple(j->_diagmN[j,i], Val(N))), Val(N))
    neighbourhood(x) = ntuple(i-> x-half_region[i], Val(N))

    # Required data structures
    TM = meantype(CT)
    region_means        =   Dict{Int, TM}()                                 # A map conatining (label, mean) pairs
    region_categories   =   Dict{Int, Int}()                                # A map conatining (label, category) pairs
    region_pix_count    =   Dict{Int, Int}()                                # A map conatining (label, count) pairs
    temp_labels         =   IS.IntDisjointSets(0)                           # Disjoint set to map labels to their equivalence class
    v_neigh             =   IS.MVector{N,Int}(undef)                        # MVector to store valid neighbours

    block_length = CartesianIndex(ntuple(i->ceil(Int,length(axes(img,i))/size(threshold,i)), Val(N)))

    for point in CartesianIndices(axes(img))
        ctg = category(img[point])
        sz = 0
        same_label = true
        prev_label = 0
        for p in neighbourhood(point)
            if checkbounds(Bool, img, p)
                root_p = IS.find_root!(temp_labels, result[p])
                if diff_fn(region_means[root_p], img[point]) < IS.getscalar(threshold, point, block_length)
                    if prev_label == 0
                        prev_label = root_p
                    elseif prev_label != root_p
                        same_label = false
                    end
                    sz += 1
                    v_neigh[sz] = IS.find_root!(temp_labels, root_p)
                end
            end
        end

        # If no valid label found
        if sz == 0 || (ctg != 0 && ctg != region_categories[prev_label])
            # Assign a new label
            new_label = push!(temp_labels)
            result[point] = new_label
            region_means[new_label] = img[point]
            region_categories[new_label] = ctg 
            region_pix_count[new_label] = 1

        # If all labels are same
        elseif same_label
            region_categories[new_label] = category(img[point])
            result[point] = prev_label
            region_pix_count[prev_label] += 1
            region_means[prev_label] += (img[point] - region_means[prev_label])/(region_pix_count[prev_label])
        else
            # Merge segments and assign to this new label
            union_label = v_neigh[1]
            for i in 1:sz
                union_label = union!(temp_labels, union_label, v_neigh[i])
            end
            result[point] = union_label
            region_pix_count[union_label] += 1
            region_means[union_label] += (img[point] - region_means[union_label])/(region_pix_count[union_label])

            for i in 1:sz
                if v_neigh[i] != union_label && haskey(region_pix_count, v_neigh[i])
                    region_pix_count[union_label] += region_pix_count[v_neigh[i]]
                    region_means[union_label] += (region_means[v_neigh[i]] - region_means[union_label])*region_pix_count[v_neigh[i]]/region_pix_count[union_label]

                    # Remove label v_neigh[i] from region_means, region_pix_count
                    delete!(region_pix_count,v_neigh[i])
                    delete!(region_means,v_neigh[i])
                end
            end
        end
    end

    for point in CartesianIndices(axes(img))
        result[point] = IS.find_root!(temp_labels, result[point])
    end

    IS.SegmentedImage(result, unique(temp_labels.parents), region_means, region_pix_count)
end
