function sum_colordiff(colors::AbstractArray{<:Colorant})
    c = mean(colors)
    sum(colordiff.(Ref(c), colors))
end
sum_colordiff(colors::AbstractArray{<:Colorant}...) = sum(map(sum_colordiff, colors))

function group_colors(colovec)
    bestinds = Int[]
    bestloss = typemax(Float64)
    start_group_size = min(10, length(colovec))
    for set_inds in powerset(1:start_group_size, 2)
        loss = sum_colordiff(colorvec[set_inds], colorvec[(1:10)[Not(set_inds)]])
        if loss < bestloss
            bestloss = loss
            bestinds = set_inds
        end
    end
    group1 = colorvec[bestinds] 
    group2 = colorvec[(1:10)[Not(bestinds)]] 
    mean1, mean2 = mean(group1), mean(group2)
    for i in (start_group_size+1):length(colorvec)
        c = colorvec[i]
        if colordiff(mean1, c) < colordiff(mean2, c)
            push!(group1, c)
        else
            push!(group2, c)
        end
    end
    return group1, group2
end
