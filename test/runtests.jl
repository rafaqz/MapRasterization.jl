using MapRasterization
using Statistics
using Test

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
