using MapRasterization
using Statistics
using GLMakie
using Colors
using Test
using ImageIO
using FileIO
using ImageView
using GLM
using Neighborhoods
using DynamicGrids
using DynamicGrids: Neighborhoods

@testset "_categorisecolor" begin
    cats = [
        (
            r = (mean=0.2, min=0.1, max=0.3, sd=0.1),
            g = (mean=0.3, min=0.2, max=0.4, sd=0.1),
            b = (mean=0.6, min=0.5, max=0.7, sd=0.1),
        ), (
            r = (mean=0.6, min=0.5, max=0.7, sd=0.1),
            g = (mean=0.3, min=0.2, max=0.4, sd=0.1),
            b = (mean=0.2, min=0.1, max=0a.3, sd=0.1),
        ),
    ]
    @test MapRasterization._categorisecolor(RGB(0.3, 0.2, 0.8), cats) == 0
    @test MapRasterization._categorisecolor(RGB(0.3, 0.2, 0.7999), cats) == 1
    @test MapRasterization._categorisecolor(RGB(0.3, 0.2, 0.7), cats) == 1
    @test MapRasterization._categorisecolor(RGB(0.6, 0.3, 0.2), cats) == 2
end


# Makie.heatmap(img)
img = load("/home/raf/PhD/Mascarenes/Data/Books/Atlas of Mauritius/3.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Atlas of Mauritius/13.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/La Reunion/20.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/49.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/52.jpg") |> rotr90

imshow(img)
imshow(tweaked)

points = MapRasterization.selectcolors(img; ncolors=15)
points = MapRasterization.selectcolors(img; points)

big_table = map(img, CartesianIndices(img)) do pixel, I
    hsl = HSL(pixel)
    map(Float64, (h=hsl.h, s=hsl.s, l=hsl.l, i=I[1], j=I[2]))
end |> vec
little_table = map(points[1]) do (i, j)
    hsl = HSL(img[round(Int, i), round(Int, j)])
    map(Float64, (; h=hsl.h, s=hsl.s, l=hsl.l, i, j))
end |> vec
model_h = lm(@formula(h ~ i + j), little_table)
model_s = lm(@formula(s ~ i + j), little_table)
model_l = lm(@formula(l ~ i + j), little_table)
pred_h = reshape(predict(model_h, big_table), size(img))
mean_h = mean(pred_h)
pred_s = reshape(predict(model_s, big_table), size(img))
mean_s = mean(pred_s)
pred_l = reshape(predict(model_l, big_table), size(img))
heatmap(pred_s)
mean_l = mean(pred_l)
tweaked = map(img, pred_h, pred_s, pred_l) do p, h, s, l 
    p1 = HSL(p)
    RGB(HSL(p1.h-h+mean_h, p1.s-s+mean_s, p1.l-l+mean_l))
end

points = MapRasterization.selectcolors(tweaked; ncolors=14)


hood = LayeredPositional(
    vert=Positional((-1, 0), (1, 0)),
    horz=Positional((0, -1), (0, 1)),
    angle45=Positional((-1, -1), (1, 1)),
    angle135=Positional((-1, 1), (1, -1)),
)

hood = LayeredPositional(
    vert=Positional((-2, 0), (-1, 0), (1, 0), (1, 0)),
    horz=Positional((0, -2), (0, -1), (0, 1), (0, 2)),
    angle45=Positional((-2, -2), (-1, -1), (1, 1), (2, 2)),
    angle135=Positional((-2, 2), (-1, 1), (1, -1), (2, -2)),
)

vert = Neighborhoods.broadcast_neighborhood(hood, img) do hood, val
    l = HSL(val).l
    dirs = map(neighbors(hood)) do layer
        sum(layer) do n
            (l - HSL(n).l)^2
        end
    end
    (dirs.vert - dirs.horz)#^2 + 
end
angle = Neighborhoods.broadcast_neighborhood(hood, img) do hood, val
    l = HSL(val).l
    dirs = map(neighbors(hood)) do layer
        sum(layer) do n
            (l - HSL(n).l)^2
        end
    end
    (dirs.angle45 - dirs.angle135)
end
blurred = Neighborhoods.broadcast_neighborhood(Window{5}(), angle) do hood, val
    mean(hood)
end
heatmap(img; alpha=0.5)
heatmap(angle)# .< -0.05)
heatmap(blurred .> 0.05)
heatmap(max.(blurred, 0).^2 - angle.^3 .> 0.01)
heatmap(classified .>  0.1)

# img = load("/home/raf/PhD/Mascarenes/Data/Scans/Map/mus_soil/mus_soils_bottom.pdf")
# seg = segments = fast_scanning(img, 0.1)
# img = map(i -> segment_mean(segments,i), labels_map(segments))
# imshow(img)


hs = vec(map(x -> x.h, img))
haist(hs; bins=2000)
vs = vec(map(x -> x.v, img))
ss = vec(map(x -> x.s, img))
scatter(hs, vs; color=vec(img))
scatter(hs, ss; color=vec(img))
scatter(vs, ss; color=vec(img))


using ImageSegmentation, FileIO
using ImageFiltering
imshow(img)
segments = felzenszwalb(img, 10, 100)  # removes segments with fewer than 100 pixels
segments = felzenszwalb(img, 3, 100)
segments = unseeded_region_growing(img, 0.05)  # removes segments with fewer than 100 pixels
segments = unseeded_region_growing(img, 0.1)
segments = prune_segments(seg, 
    i -> (segment_pixel_count(seg, i) < 10), 
    (i, j) -> (-segment_pixel_count(seg, j))
)
segments = prune_segments(seg, 
    i -> (MapRasterization._categorisecolor(segment_mean(seg, i), cats) == 0 && segment_pixel_count(seg, i) < 5), 
 a   (i, j) -> (-segment_pixel_count(seg, j))
)

points2 = MapRasterization.selectcolors(img; points, ncolors=10)

using ProfileView
@profview 
Makie.heatmap(rotr90(filt))

sort(vec(map(i->segment_mean(segments,i), labels_map(segments))); by=x->x.r) |> union

fig = Figure()
b = Button(fig, label="x")
