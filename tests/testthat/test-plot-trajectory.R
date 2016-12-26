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
  labels <- nodes %>% sub(".*label = '", "", .) %>% sub("'.*", "", .)
  ids <- nodes %>% sub("[[:space:]]*'", "", .) %>% sub("' \\[label.*", "", .)
  for (i in 1:length(ids))
    graph <- gsub(ids[[i]], i, graph)
  graph_lines <- strsplit(graph, "\n")[[1]]
  edges <- graph_lines[grep("->", graph_lines)]
  edges <- edges %>% sub(" \\[.*", "", .) %>% gsub("'", "", .)
  edges <- strsplit(edges, "->") %>%
    as.data.frame %>% t %>%
    as.data.frame(stringsAsFactors = FALSE)
  from <- as.numeric(edges$V1)
  to <- as.numeric(edges$V2)

  expect_true(all(from == c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 18, 4, 5, 5, 9, 4, 17, 19)))
  expect_true(all(to == c(2, 3, 4, 18, 18, 7, 8, 18, 11, 18, 13, 14, 15, 16, 17, 19, 5, 6, 9, 10, 12, 1, 4)))
})
