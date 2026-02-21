#' Move all points slightly and recalculate distances,
#' except for points (13, 1) and (18, 3) as these are
#' connected to the curved edge in the example.
#' 
#' Points with the same x and y coordinates will always receive the same jitter.
#' 
#' @export
jitter_graph = function (G, amt) {

    # Move all the points slightly, except those connected to a curved edge
    vs = V(G)[!(
        (V(G)$x == 13 & V(G)$y == 1) |
        (V(G)$x == 18 & V(G)$y == 3) 
    )]

    jit_x = vapply(1:length(vs), \(i) {
        set.seed(V(G)[vs[i]]$x * 11 + V(G)[vs[i]]$y * 13)
        runif(1, -amt, amt)
    }, 0.0)

    jit_y = vapply(1:length(vs), \(i) {
        set.seed(V(G)[vs[i]]$x * 17 + V(G)[vs[i]]$y * 19)
        runif(1, -amt, amt)
    }, 0.0)

    V(G)[vs]$x = V(G)[vs]$x + jit_x
    V(G)[vs]$y = V(G)[vs]$y + jit_y

    # and update the lengths
    for (edge in E(G)[E(G)$curvature == 0]) {
        fr = V(G)[.from(edge)]
        to = V(G)[.to(edge)]
        stopifnot(length(fr) == 1)
        stopifnot(length(to) == 1)
        E(G)[edge]$weight = sqrt((V(G)[fr]$x - V(G)[to]$x)^2 + (V(G)[fr]$y - V(G)[to]$y)^2)
    }

    G
}