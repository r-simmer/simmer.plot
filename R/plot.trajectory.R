#' Plot method for trajectory objects
#'
#' A method for the \code{\link{plot}} generic that plots a diagram of the given trajectory.
#'
#' @param x a simmer trajectory.
#' @param engine a string specifying a layout engine (see \code{\link{grViz}}).
#' @param ... additional parameters for \code{\link{render_graph}}.
#'
#' @return Returns an \code{htmlwidget}.
#' @importFrom graphics plot
#' @export
#'
#' @examples
#' x <- trajectory() %>%
#' seize("resource", 1) %>%
#'   timeout(function() rnorm(1, 15)) %>%
#'   release("resource", 1) %>%
#'   branch(function() 1, c(TRUE, FALSE),
#'          trajectory() %>%
#'            clone(2,
#'                  trajectory() %>%
#'                    seize("resource", 1) %>%
#'                    timeout(1) %>%
#'                    release("resource", 1),
#'                  trajectory() %>%
#'                    trap("signal",
#'                         handler=trajectory() %>%
#'                           timeout(1)) %>%
#'                    timeout(1)),
#'          trajectory() %>%
#'            set_attribute("dummy", 1) %>%
#'            set_attribute("dummy", function() 1) %>%
#'            seize("resource", function() 1) %>%
#'            timeout(function() rnorm(1, 20)) %>%
#'            release("resource", function() 1) %>%
#'            rollback(9)) %>%
#'   timeout(1) %>%
#'   rollback(2)
#'
#' plot(x)
plot.trajectory <- function(x, engine="dot", ...) {
  stopifnot(length(x) > 0)

  trajectory_graph(x) %>%
    DiagrammeR::set_global_graph_attrs("layout", engine, "graph") %>%
    DiagrammeR::render_graph(...)
}

trajectory_graph <- function(x) {
  # capture output with pointers
  old_verbose <- x$verbose
  x$verbose <- TRUE
  out <- gsub("\b", "", utils::capture.output(x))
  x$verbose <- old_verbose
  out <- out[grep("0x", out)]
  if (!length(out))
    stop("no activity pointers found! \n",                                                      # nocov
         "  This is embarrassing... The trajectory cannot be plotted without pointers!\n",      # nocov
         "  Please, consider filling a bug at https://github.com/r-simmer/simmer.plot/issues")  # nocov

  # assign reproducible identifiers
  ids <- sub(" ->.*", "", sub(".*<- ", "", out))
  for (i in seq_along(ids)) out <- gsub(ids[i], i, out)

  # find forks & rollbacks
  level <- nchar(sub("\\{.*", "", out)) / 2
  forks <- which(diff(level) == 1)
  rollbacks <- grep("Rollback", out)
  # find activity names
  out <- sub(".*Activity: ", "", out)
  nodes <- as.data.frame(sub(" .*", "", out), stringsAsFactors=FALSE)
  colnames(nodes) <- "label"
  nodes$type <- nodes$label
  nodes$shape <- "box"
  if (length(c(forks, rollbacks)))
    nodes[c(forks, rollbacks),]$shape <- "diamond"

  # back connections
  out <- sub("[[:alpha:]]*[[:space:]]*\\| ", "", out)
  b_edges <- suppressWarnings(
    sub(" ->.*", "", out) %>%
      strsplit(" <- ") %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame(stringsAsFactors=FALSE)
  )
  colnames(b_edges) <- c("from", "to")
  nodes$id <- b_edges$to
  b_edges <- utils::tail(b_edges, -1)
  rownames(b_edges) <- NULL

  # forward connections
  out <- sub(".*<- ", "", out)
  f_edges <- suppressWarnings(
    sub(" \\|.*", "", out) %>%
      strsplit(" -> ") %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame(stringsAsFactors=FALSE)
  )
  rownames(f_edges) <- NULL
  colnames(f_edges) <- c("from", "to")
  f_edges <- f_edges[f_edges$to != 0 & !is.na(f_edges$to),]

  # additional info & rollbacks
  out <- sub(".*\\| ", "", out)
  info <- sub(" \\}", "", out)
  info[rollbacks] <- sub("amount: ", "", info[rollbacks])
  amounts <- as.numeric(sub(" \\(.*", "", info[rollbacks]))
  info[rollbacks] <- sub(".*, ", "", info[rollbacks])
  nodes$tooltip <- info

  # resolve rollbacks from back connections
  r_edges <- NULL
  graph <- DiagrammeR::create_graph(nodes, b_edges)
  for (i in seq_along(amounts)) {
    from <- nodes[rollbacks[i],]$id
    graph <- DiagrammeR::select_nodes_by_id(graph, from)
    try({
      for (j in seq_len(amounts[i]))
        graph <- DiagrammeR::trav_in(graph)
    }, silent = TRUE)
    to <- as.numeric(DiagrammeR::get_selection(graph))
    graph <- DiagrammeR::clear_selection(graph)
    r_edges <- rbind(r_edges, data.frame(from=from, to=to))
  }

  # compose edges
  edges <- unique(rbind(f_edges, b_edges, r_edges))
  if (nrow(edges)) {
    edges$id <- 1:nrow(edges)
    edges$rel <- NA
    edges$color <- "black"
    edges$style <- "solid"
    if (length(forks)) {
      edges[c(forks),]$color <- "gray"
      edges[c(forks),]$style <- "dashed"
    }
  } else edges <- NULL

  DiagrammeR::create_graph(nodes, edges)
}
