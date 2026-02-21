test_that("gridded_graph works", {
  for (nr in 2:10) {
    for (nc in 2:10) {
      for (xoff in 0:2) {
        for (yoff in 0:2) {
          for (voff in 0:2) {
            G = gridded_graph(nr, nc, vidoffset=voff, xoffset=xoff, yoffset=yoff)
            
            expect_equal(length(V(G)), nr * nc)
            
            expect_equal(length(E(G)),
              # horizontal edges
              nr * (nc - 1) +
              # vertical edges
              nc * (nr - 1)
            )

            edges = igraph::as_data_frame(G, what="edges")
            vertices = igraph::as_data_frame(G, what="vertices")

            edges = edges |>
              left_join(rename_with(vertices, \(x) glue::glue("{x}_from")), by=c("from"="name_from")) |>
              left_join(rename_with(vertices, \(x) glue::glue("{x}_to")), by=c("to"="name_to"))

            expect_all_true(
              with(edges,
                x_from == x_to & y_from == y_to - 1 |
                x_from == x_to - 1 & y_from == y_to
              )
            )

            expect_equal(min(as.numeric(vertices$name)), voff + 1)
            expect_equal(min(vertices$x), xoff + 1)
            expect_equal(min(vertices$y), yoff + 1)

            # for historical reasons x and y are reversed here
            expect_true(!anyDuplicated(get_grid_vid(vertices$row, vertices$col, nc)))
            expect_all_true(as.numeric(vertices$name) == get_grid_vid(vertices$row, vertices$col, nc) + voff)
          }
        }
      }
    }
  }
})


test_that("diagon_alley works",  {
  for (sz in 3:10) {
    for (p1 in 2:(sz-1)) {
      for (p2 in 2:(sz-1)) {
        G = diagon_alley(sz, p1, p2)

        expect_equal(length(V(G)), sz ^ 2 - 1)
        expect_equal(length(E(G)),
          # 4 removed edges, one added
          2 * sz * (sz - 1) - 4 + 1
        )

        # Make sure the right vertex is missing
        vertices = igraph::as_data_frame(G, what="vertices")

        expect_all_false(vertices$x == p1 & vertices$y == p2)

        # and the edge should be added
        expect_equal(
          length(E(G)[
            .inc(V(G)[as.character(get_grid_vid(p1 - 1, p2 - 1, sz))]) &
            .inc(V(G)[as.character(get_grid_vid(p1 + 1, p2 + 1, sz))])]),
            1
        )
      }
    }
  }
})