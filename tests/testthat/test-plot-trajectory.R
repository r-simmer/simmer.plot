context("plot.trajectory")

test_graph <- function(x, name, from, to) {
  expect_true(inherits(plot(x), "htmlwidget"))
  graph <- trajectory_graph(x)
  expect_true(inherits(graph, "dgr_graph"))
  dot <- DiagrammeR::generate_dot(graph)
  expect_true(inherits(dot, "character"))
  expect_true(length(dot) == 1)
  graph_lines <- strsplit(dot, "\n")[[1]]

  nodes <- graph_lines[grep("label", graph_lines)]
  id <- nodes %>%
    sub("[[:space:]]*'", "", .) %>%
    sub("' \\[.*", "", .) %>%
    as.numeric()
  label <- nodes %>%
    sub(".*label = '", "", .) %>%
    sub("'.*", "", .)
  nodes <- data.frame(id=id, label=label) %>%
    dplyr::arrange_("id")

  edges <- graph_lines[grep("->", graph_lines)] %>%
    sub(" \\[.*", "", .) %>%
    gsub("'", "", .) %>%
    strsplit("->") %>%
    lapply(as.numeric) %>%
    as.data.frame %>% t %>%
    as.data.frame(stringsAsFactors = FALSE)
  if (nrow(edges)) {
    rownames(edges) <- NULL
    colnames(edges) <- c("from", "to")
    edges <- dplyr::arrange_(edges, "from", "to")
  } else edges <- NULL

  expect_true(all(nodes$label == name))
  if (!is.null(edges)) {
    expect_true(all(edges$from == from))
    expect_true(all(edges$to == to))
  } else {
    expect_null(from)
    expect_null(to)
  }
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
    seize("resource", 1) %>%
    release("resource", 1)

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
    seize("resource", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    release("resource", 1) %>%
    branch(function() 1, c(TRUE, FALSE),
           trajectory() %>%
             clone(2,
                   trajectory() %>%
                     seize("resource", 1) %>%
                     timeout(1) %>%
                     release("resource", 1),
                   trajectory() %>%
                     trap("signal",
                          handler=trajectory() %>%
                            timeout(1)) %>%
                     timeout(1)),
           trajectory() %>%
             set_attribute("dummy", 1) %>%
             set_attribute("dummy", function() 1) %>%
             seize("resource", function() 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("resource", function() 1) %>%
             rollback(9)) %>%
    timeout(1) %>%
    rollback(2)

  test_graph(x,
             c("Seize", "Timeout", "Release", "Branch", "Clone", "Seize", "Timeout",
               "Release", "Trap", "Timeout", "Timeout", "SetAttribute", "SetAttribute",
               "Seize", "Timeout", "Release", "Rollback", "Timeout", "Rollback"),
             c(1, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 8, 9, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19),
             c(2, 3, 4, 5, 12, 18, 6, 9, 18, 7, 8, 18, 10, 11, 18, 13, 14, 15, 16, 17, 1, 19, 4))
})
