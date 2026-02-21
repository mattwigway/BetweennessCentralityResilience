gridded_graph = function (nrow, ncol, vidoffset=0, xoffset=0, yoffset=0) {
    vertices = tibble(row=1:nrow) |>
        cross_join(tibble(col=1:ncol)) |>
        mutate(vid = get_grid_vid(row, col, ncol) + vidoffset) |>
        relocate(vid) |>
        # Note that row and col are actually backwards here, but that makes the plots more visually appealing
        # and doesn't affect the analysis.
        mutate(x=row + xoffset, y=col + yoffset)

    # make the edges - all the ones from a vertex to the same vertex in next row/col
    edges = rbind(
        vertices |>
            filter(row < nrow) |>
            mutate(vid2 = get_grid_vid(row + 1, col, ncol) + vidoffset, weight=1, curvature=0),
        vertices |>
            filter(col < ncol) |>
            mutate(vid2 = get_grid_vid(row, col + 1, ncol) + vidoffset, weight=1, curvature=0)
    ) |>
    select(vid, vid2, weight, curvature)

    return(graph_from_data_frame(edges, directed=F, vertices=vertices))

}

get_grid_vid = function (row, col, ncol) {
    return((row - 1) * ncol + col)
}