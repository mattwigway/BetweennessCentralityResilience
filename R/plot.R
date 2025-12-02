#' @export
plot_graph = function (G, value=NULL) {
    vertices = tibble(
        vid = vertex_attr(G, "name"),
        x = vertex_attr(G, "x"),
        y = vertex_attr(G, "y")
    )

    edges = as_tibble(as_edgelist(G))
    names(edges) = c("vid_start", "vid_end")
    edges$value = value
    edges$curvature = edge_attr(G, "curvature")
    edges = edges |>
        left_join(rename_with(vertices, \(x) glue("{x}_start")), by="vid_start") |>
        left_join(rename_with(vertices, \(x) glue("{x}_end")), by="vid_end") 
    
    if (!is.null(value)) {
        p = ggplot() + lapply(1:nrow(edges), \(i) geom_curve(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, color=value), data=edges[i,], curvature=edges$curvature[[i]], linewidth=3, lineend="round"))
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