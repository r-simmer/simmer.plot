context("plot.trajectory")

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

  expect_true(inherits(plot(x), "htmlwidget"))

  graph <- plot(x, output="DOT")
  graph_lines <- strsplit(graph, "\n")[[1]]

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
  rownames(edges) <- NULL
  colnames(edges) <- c("from", "to")
  edges <- dplyr::arrange_(edges, "from", "to")

  expect_true(all(nodes$label == c("Seize", "Timeout", "Release", "Branch", "Clone", "Seize", "Timeout",
                                   "Release", "Trap", "Timeout", "Timeout", "SetAttribute", "SetAttribute",
                                   "Seize", "Timeout", "Release", "Rollback", "Timeout", "Rollback")))
  expect_true(all(edges$from == c(1, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 8, 9, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19)))
  expect_true(all(edges$to == c(2, 3, 4, 5, 12, 18, 6, 9, 18, 7, 8, 18, 10, 11, 18, 13, 14, 15, 16, 17, 1, 19, 4)))
})
