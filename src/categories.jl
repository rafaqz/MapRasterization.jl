# Generate statistics for all components in the pixels of A
function _component_stats(A, pointvecs::AbstractArray{<:AbstractArray})
    _component_stats.(Ref(A), pointvecs, eachindex(pointvecs))
end
# function _component_statalc_category_statss(A, pointvec::AbstractArray, cat::Int)
#     length(pointvec) == 0 && return missing
#     colors = _point_values(A, pointvec)
#     color_components = map(_keysnamedtuple(first(colors))) do k
#         [getfield(c, k) for c in colors]
#     end
#     # Ouputput a NamedTuple of NamedTuple, like:
#     # (r=(mean=?, min=?, max=?, sd=?, g=(mean=?...
#     map(color_components) do c
#         (; category=cat, mean=mean(c), min=minimum(c), max=maximum(c), sd=std(c))
#     end
# end
function _component_stats(A::AbstractArray, pointvec::AbstractArray, cat::Int)
    length(pointvec) == 0 && return missing
    pixels = _point_values(A, pointvec)
    return _component_stats(pixels)
end

function _component_stats(pixels::Vector{<:PixelProp})
    (
        color=_component_stats([x.color for x in pixels]),
        std=_component_stats([x.std for x in pixels]),
        stripe=_component_stats([x.stripe for x in pixels]),
    )
end
function _component_stats(colors::Vector{<:Colors.Colorant})
    map(_keysnamedtuple(first(colors))) do k
        _component_stats([getfield(c, k) for c in colors])
    end
end
function _component_stats(xs::Vector{<:NTuple{N}}) where N
    ntuple(N) do i
        _component_stats([x[i] for x in xs])
    end
end
function _component_stats(x::Vector{<:Union{Real,Colorant}})
    (; mean=mean(x), min=minimum(x), max=maximum(x), sd=std(x))
end


function _meancolors(::Type{T}, categories) where T <: PixelProp
    map(categories) do ctg
        _colortype(T)(map(x -> x.mean, ctg.color)...)
    end
end
function _meancolors(::Type{C}, categories) where C <: Colorant
    map(categories) do ctg
        C(map(x -> x.mean, ctg.color)...)
    end
end

# Categorize an array of pixels
function _categorize(pixels::AbstractArray, segments, points, scan_threshold, tolerance)
    categories = collect(skipmissing(_component_stats(pixels, points)))
    meancolors = _meancolors(eltype(pixels), categories)
    ctg_segments = map(enumerate(points)) do (ctg, pointvec)
        map(pointvec) do P
            [begin
                I = map(p -> round(Int, p), P) 
                N = I[1] + i, I[2] + j
                segments.image_indexmap[N...] => ctg
             end for i in -1:1, j in -1:1]
        end |> vcat
    end
    known_segment_categories = Dict(vcat(ctg_segments...))
    category_ints = map(enumerate(pixels)) do (i, pix)
        seg = segments.image_indexmap[i]
        ctg = get(known_segment_categories, seg, 0)
        ctg == 0 ? _categorize(pix, categories; tolerance) : ctg
    end
    category_img = map(category_ints) do ctg
        ctg == 0 ? RGBA(1.0, 1.0, 0.0, 0.0) : RGBA(1 - (categories[ctg].stripe[1].mean + 1)/3, 1 - (categories[ctg].stripe[2].mean + 1)/3, 0, 1)
    end
    category_img, category_ints
end
# Categorize a pixel
function _categorize(x, categories; tolerance=2.0)
    errs = map(categories) do cat
        _category_error(x, cat)
    end
    _, best = findmin(errs)
    return _isincategory(x, categories[best], tolerance) ? best : 0
end


function _category_error(val::PixelProp, stats)
    sum(map(_category_error, _asnamedtuple(val), stats))
end
function _category_error(v::HSL, s)
    0#((v.h - s.h.mean)/100)^2 + (v.s - s.s.mean)^2 + (v.l - s.l.mean)^2 
end
function _category_error(val::Colorant, stats)
    # sum(map(_asnamedtuple(val), stats) do v, s 
    #     (v - s.mean)^2
    # end)
    0
end
function _category_error(val::Tuple, stats)
    sum(map(val, stats) do v, s 
        (v - s.mean)^2
    end)
end
_category_error(v::Number, s) = (v - s.mean)^2

function _isincategory(val::RGBA, stats, tol)
    val.alpha == 0 && return false
    _isincategory(RGB(val), stats[(:r, :g, :b)], tol)
end
function _isincategory(val::PixelProp, stats::NamedTuple{(:color,:std,:stripe)}, tol)
    all(map((v, s) -> _isincategory(v, s, tol), _asnamedtuple(val), stats))
end
function _isincategory(val::RGB, stats::NamedTuple{(:r,:g,:b)}, tol)
    all(map((v, s) -> _isincategory(v, s, tol), _asnamedtuple(val), stats))
end
function _isincategory(vals::Tuple, stats::Tuple, tol)
    all(map((v, s) -> _isincategory(v, s, tol), vals, stats))
end
function _isincategory(val::HSL, stats::NamedTuple{(:h,:s,:l)}, tol)
    isincats = map(_asnamedtuple(val)[(:h, :s)], categories[best][(:h, :s)]) do v, s
        _isincategory(v, s, tol)
    end 
    return all(isincats)
end
function _isincategory(v::Real, s, tol)
    (v >= (s.min - s.sd * tol)) && (v <= (s.max + s.sd * tol))
end


# Utils
function _asnamedtuple(x::T) where T
    fnames = fieldnames(T)
    NamedTuple{fnames}(map(fn -> getfield(x, fn), fnames))
end

_keysnamedtuple(nt::NamedTuple) = NamedTuple{keys(nt)}(keys(nt))
_keysnamedtuple(x) = _keysnamedtuple(_asnamedtuple(x))
_keysnamedtuple(::Type{T}) where T = NamedTuple{fieldnames(T)}

_colortype(pixels::AbstractArray) = _colortype(eltype(pixels))
_colortype(x) = _colortype(typeof(pixels))
_colortype(::Type{C}) where {C<:Colorant} = C
_colortype(::Type{P}) where {P<:PixelProp{C}} where C = C

_point_index(P) = map(p -> round(Int, p), P)
_point_value(A::AbstractMatrix, P) = A[_point_index(P)...]
_point_values(A::AbstractMatrix, pointvec::AbstractVector) = map(P -> _point_value(A, P), pointvec)
