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
selected_dir = "/home/raf/PhD/Mascarenes/Data/Selected"

img_path = joinpath(selected_dir, "Mauritius/Undigitised/3.jpg")
img_path = joinpath(selected_dir, "Mauritius/Undigitised/13.jpg")
img_path = joinpath(selected_dir, "Mauritius/Undigitised/6.jpg")
img_path = joinpath(selected_dir, "Mauritius/Undigitised/49.jpg")
img_path = joinpath(selected_dir, "Mauritius/Undigitised/50.jpg")
img_path = joinpath(selected_dir, "Mauritius/Undigitised/52.jpg")

img = load(img_path) |> rotr90
json_path = splitext(img_path)[1] * ".json"
if isfile(json_path)
    output = JSON3.read(read(json_path), MapRasterization.MapSelection)
    output = MapRasterization.selectcolors(img, output)
else
    output = MapRasterization.selectcolors(img; ncategories=15)
end
write(json_path, JSON3.write(output))

img_paths = map(s -> joinpath(selected_dir, s), (
    "Mauritius/Undigitised/3.jpg",
    "Mauritius/Undigitised/13.jpg",
    "Mauritius/Undigitised/6.jpg",
    "Mauritius/Undigitised/49.jpg",
    "Mauritius/Undigitised/50.jpg",
    "Mauritius/Undigitised/52.jpg"
))

for img_path in img_paths
    img = load(img_path) |> rotr90
    json_path = splitext(img_path)[1] * ".json"
    if isfile(json_path)
        output = JSON3.read(read(json_path), MapRasterization.MapSelection)
        output = MapRasterization.selectcolors(img, output)
    else
    output = MapRasterization.selectcolors(img; ncategories=15)
    # end
    write(json_path, JSON3.write(output))
    sleep(2)
end

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
