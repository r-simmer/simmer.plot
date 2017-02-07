context("plot.trajectory")

test_graph <- function(x, name, from, to) {
  expect_true(inherits(plot(x), "htmlwidget"))
  graph <- trajectory_graph(x, scales::brewer_pal("qual"))
  expect_true(inherits(graph, "dgr_graph"))
  expect_true(all(graph$nodes_df$label == name))
  expect_true(all(graph$edges_df$from == from))
  expect_true(all(graph$edges_df$to == to))
}

test_that("a null trajectory fails", {
  expect_error(plot(trajectory()))
})

test_that("a single-node trajectory is correctly converted to graph", {
  x <- trajectory() %>%
    timeout(1)

  test_graph(x, c("Timeout"), NULL, NULL)
})

test_that("a simple trajectory is correctly converted to graph", {
  x <- trajectory() %>%
    seize("res", 1) %>%
    release("res", 1)

  test_graph(x, c("Seize", "Release"), 1, 2)
})

test_that("a rollback with variable amount is correctly converted to graph", {
  test_graph(trajectory() %>% rollback(0), c("Rollback"), 1, 1)
  test_graph(trajectory() %>% rollback(1), c("Rollback"), 1, 1)
  test_graph(trajectory() %>% timeout(1) %>% rollback(0), c("Timeout", "Rollback"), c(1, 2), c(2, 2))
  test_graph(trajectory() %>% timeout(1) %>% rollback(1), c("Timeout", "Rollback"), c(1, 2), c(2, 1))
  test_graph(trajectory() %>% timeout(1) %>% rollback(2), c("Timeout", "Rollback"), c(1, 2), c(2, 1))
})

test_that("a complex trajectory is correctly converted to graph", {
  x <- trajectory() %>%
    seize("res0", 1) %>%
    branch(function() 1, c(TRUE, FALSE),
           trajectory() %>%
             clone(2,
                   trajectory() %>%
                     seize("res1", 1) %>%
                     timeout(1) %>%
                     release("res1", 1),
                   trajectory() %>%
                     trap("signal",
                          handler=trajectory() %>%
                            timeout(1)) %>%
                     timeout(1)),
           trajectory() %>%
             set_attribute("dummy", 1) %>%
             seize("res2", function() 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("res2", function() 1) %>%
             release("res0", 1) %>%
             rollback(11)) %>%
    synchronize() %>%
    rollback(2) %>%
    release("res0", 1)

  test_graph(x,
             c("Seize", "Branch", "Clone", "Seize", "Timeout", "Release", "Trap",
               "Timeout", "Timeout", "SetAttribute", "Seize", "Timeout", "Release",
               "Release", "Rollback", "Synchronize", "Rollback", "Release"),
             c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 16, 17, 2, 3, 3, 7, 2, 15, 17),
             c(2, 16, 16, 5, 6, 16, 9, 16, 11, 12, 13, 14, 15, 17, 18, 3, 4, 7, 8, 10, 1, 2))
})
