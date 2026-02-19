#' Implements the algorithm in
#' Cardwell, J., Delamater, P. L., & Konrad, C. E. (2026). A population-based,
#' demand-aware framework for measuring structural road network redundancy.
#' Journal of Transport Geography, 131, 104569. https://doi.org/10.1016/j.jtrangeo.2026.104569
#' for an igraph network
#' @export 
cardwell_redundancy = function (G, origin, dests, weights, penalty_factor, iters=15, progress=TRUE) {
    penalized = rep(FALSE, length(weights))
    result = list()
    for (i in 1:iters) {
        penalized_weights = ifelse(penalized, weights * penalty_factor, weights)

        paths = shortest_paths(G, origin, dests, output="both", weights=penalized_weights)

        # compute aggregate weight
        agg_weight = 0
        for (path in paths$epath) {
            pathv = as.vector(path)
            agg_weight = agg_weight + sum(penalized_weights[pathv])
            # make sure they are penalized next time around
            penalized[pathv] = TRUE
        }

        result[[i]] = list(
            paths = paths,
            agg_weight = agg_weight
        )
    }

    # linear extrapolation
    lowest = result[[1]]$agg_weight
    highest = result[[iters]]$agg_weight

    max_deviation = 0
    score = 0

    for (i in 1:iters) {
        # fencepost problem 15 iters is 14 jumps
        result[[i]]$exp_weight = lowest + (highest - lowest) / (iters - 1) * (i - 1)
        result[[i]]$deviation = result[[i]]$agg_weight - result[[i]]$exp_weight

        if (result[[i]]$deviation > max_deviation) {
            max_deviation = result[[i]]$deviation
            score = i
        }
    }

    return(list(score=score, iterations=result))
}
