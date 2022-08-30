using RasterUtils
using GLMakie
using Colors
using Test
using ImageIO
using FileIO
using ImageView

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
    @test RasterUtils._categorisecolor(RGB(0.3, 0.2, 0.8), cats) == 0
    @test RasterUtils._categorisecolor(RGB(0.3, 0.2, 0.7999), cats) == 1
    @test RasterUtils._categorisecolor(RGB(0.3, 0.2, 0.7), cats) == 1
    @test RasterUtils._categorisecolor(RGB(0.6, 0.3, 0.2), cats) == 2
end

# Makie.heatmap(img)
img = load("/home/raf/PhD/Mascarenes/Data/Books/Atlas of Mauritius/13.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/La Reunion/2.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/49.jpg") |> rotr90
img = load("/home/raf/PhD/Mascarenes/Data/Books/Forests of Mauritius/Maps/52.jpg") |> rotr90

points = RasterUtils.selectcolors(img; ncolors=14)
points = RasterUtils.selectcolors(img; points)

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
    i -> (RasterUtils._categorisecolor(segment_mean(seg, i), cats) == 0 && segment_pixel_count(seg, i) < 5), 
 a   (i, j) -> (-segment_pixel_count(seg, j))
)

points2 = RasterUtils.selectcolors(img; points, ncolors=10)

using ProfileView
@profview 
Makie.heatmap(rotr90(filt))

sort(vec(map(i->segment_mean(segments,i), labels_map(segments))); by=x->x.r) |> union

fig = Figure()
b = Button(fig, label="x")
