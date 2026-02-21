diagon_alley = function (size, target_x, target_y) {
    gridded_graph(size, size) +
        # add the diagonal edge
        edge(get_grid_vid(target_x - 1, target_y - 1, size), get_grid_vid(target_x + 1, target_y + 1, size), weight = 2 * sqrt(2), curvature=0) -
        # delete the central vertex
        vertex(get_grid_vid(target_x, target_y, size))
}