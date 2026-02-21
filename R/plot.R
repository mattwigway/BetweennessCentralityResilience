#' Plot a graph, optionally adjusting colors or line widths based on
#' some vector of edge attributes.
#' @export
plot_graph = function (G, value=NULL, linewidth=NULL) {
    vertices = igraph::as_data_frame(G, what="vertices")
    edges = igraph::as_data_frame(G, what="edges") |>
        rename(name_start="from", name_end="to")
    edges$value = value
    edges$linewidth = linewidth
    edges$curvature = edge_attr(G, "curvature")
    edges = edges |>
        left_join(rename_with(vertices, \(x) glue("{x}_start")), by="name_start") |>
        left_join(rename_with(vertices, \(x) glue("{x}_end")), by="name_end") 
    
    if (!is.null(value)) {
        p = ggplot() + lapply(1:nrow(edges), \(i) geom_curve(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, color=value), data=edges[i,], curvature=edges$curvature[[i]], linewidth=3, lineend="round"))
    } else if (!is.null(linewidth)) {
        p = ggplot() + lapply(1:nrow(edges), \(i) geom_curve(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, linewidth=linewidth, alpha=linewidth), data=edges[i,], curvature=edges$curvature[[i]], lineend="round"))
    } else {
        # no color and legend
        p = ggplot() + lapply(1:nrow(edges), \(i) geom_curve(aes(x=x_start, y=y_start, xend=x_end, yend=y_end), data=edges[i,], curvature=edges$curvature[[i]], color="gray", linewidth=3, lineend="round"))
    }


    p = p +
        geom_point(aes(x=x, y=y), data=vertices) +
        theme_minimal() +
        coord_fixed() +
        theme(
            axis.title=element_blank(),
            axis.text=element_blank(),
            panel.grid=element_blank()
        )

    return(p)
}