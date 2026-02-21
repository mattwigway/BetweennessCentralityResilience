test_that("jitter works", {
    G = gridded_graph(13, 13)
    J = jitter_graph(G, 0.01)

    Gv = igraph::as_data_frame(G, what="vertices")
    Jv = igraph::as_data_frame(J, what="vertices")

    expect_all_true(abs(Gv$x - Jv$x) <= 0.01)
    expect_equal(Jv[Jv$x == 13 & Jv$y == 1, "x"], 13)
    expect_equal(Jv[Jv$x == 13 & Jv$y == 1, "y"], 1)

    # all the edges should have expected length
    edges = igraph::as_data_frame(G, what="edges")
    vertices = igraph::as_data_frame(G, what="vertices")

    edges = edges |>
        left_join(rename_with(vertices, \(x) glue::glue("{x}_from")), by=c("from"="name_from")) |>
        left_join(rename_with(vertices, \(x) glue::glue("{x}_to")), by=c("to"="name_to"))

    expect_all_true(edges$weight == sqrt((edges$x_from - edges$x_to)^2 + (edges$y_from - edges$y_to)^2))
})