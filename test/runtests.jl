using MapRasterization
using Statistics
using GLMakie
using Colors
using Test
using ImageIO
using FileIO
using ImageView
using GLM
using DynamicGrids
using DynamicGrids: Neighborhoods
using ImageCore
using JSON3
a
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
img = load("/home/raf/PhD/Mascarenes/Data/Books/Atlas of Mauritius/6.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/La Reunion/20.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/49.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/50.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/52.jpg") |> rotr90

output = MapRasterization.selectcolors(img; ncategories=10)
output = MapRasterization.selectcolors(img, output)
write("13.json", JSON3.write(output))
output = JSON3.read(read("output.json"), MapRasterization.MapSelection)
imshow(output.output)
imshow(nolines)
x = RGBA(img[1], 0)

stripes = MapRasterization._stripes(img; radius=3)
stds = Neighborhoods.broadcast_neighborhood(Window{2}(), img) do hood, val
    rs = std(map(n -> n.r, hood))
    gs = std(map(n -> n.g, hood))
    bs = std(map(n -> n.b, hood))
    sum((rs, gs, bs))
end
cstds = Neighborhoods.broadcast_neighborhood(Window{8}(), stds) do hood, val
    count(>(0.05), hood) >= length(hood) * 0.7 
end
heatmap(cstds)
heatmap(stds)
bstripes = Neighborhoods.broadcast_neighborhood(Window{1}(), last.(stripes)) do hood, val
    mean(hood)
end
# heatmap(last.(stripes))
fig = Figure()
ax1 = Axis(fig[1, 1]; title="Source")
ax2 = Axis(fig[1, 2]; title="Stripes")
ax3 = Axis(fig[1, 3]; title="Stripes")
linkaxes!(ax1, ax2, ax3)
heatmap!(ax1, img)
heatmap!(ax2, stds)
heatmap!(ax3, stds .> 0.05)

heatmap!(ax3, bstripes .> 0.002)

# heatmap!(ax3, nolines)
RGBA(RGB(1.0), 1) + RGBA(RGB(1), 1) 

stripes = MapRasterization._stripes(img; radius=3)
heatmap((x -> (x < -0.01) & (x > -0.1)).(last.(stripes)))
blurred = Neighborhoods.broadcast_neighborhood(Window{2}(), last.(stripes)) do hood, val
    mean(hood)
end
heatmap(blurred)
heatmap((x -> (x < -0.005) & (x > -0.05)).(blurred))


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
a
sort(vec(map(i->segment_mean(segments,i), labels_map(segments))); by=x->x.r) |> union

fig = Figure()
b = Button(fig, label="x")

hood = MapRasterization.cross_layer_neig1borhood(2)
A = (1:2:20) * (1:10)'
A = Neighborhoods.pad_array(A, hood)
hood = Neighborhoods.updatewindow(hood, A, 1, 1)
hood._window
neighbors(hood)

g1, g2 = group_colors(colorvec)
g1
g2
using Colors: N0f8

x = Float64.(reinterpret(reshape, N0f8, reinterpret(Tuple{N0f8,N0f8,N0f8}, vec(img))))
r = kmeans(x, 10)

im = reshape(r.assignments, size(img))
heatmap(im)
heatmap(img)
