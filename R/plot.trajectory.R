#' Plot method for trajectory objects
#'
#' A method for the \code{\link{plot}} generic that plots a diagram of the given trajectory.
#'
#' @param x a simmer trajectory.
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
plot.trajectory <- function(x, ...) {
  # capture output with pointers
  old_verbose <- x$verbose
  x$verbose <- TRUE
  out <- gsub("\b", "", utils::capture.output(x))
  x$verbose <- old_verbose
  out <- out[grep("0x", out)]

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
  nodes[c(forks, rollbacks),]$shape <- "diamond"

  # back connections
  out <- sub("[[:alpha:]]*[[:space:]]*\\| ", "", out)
  b_edges <- sub(" ->.*", "", out) %>%
    strsplit(" <- ") %>%
    lapply(as.numeric) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame(stringsAsFactors=FALSE)
  colnames(b_edges) <- c("from", "to")
  nodes$nodes <- b_edges$to
  b_edges <- utils::tail(b_edges, -1)
  rownames(b_edges) <- NULL

  # forward connections
  out <- sub(".*<- ", "", out)
  f_edges <- sub(" \\|.*", "", out) %>%
    strsplit(" -> ") %>%
    lapply(as.numeric) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame(stringsAsFactors=FALSE)
  rownames(f_edges) <- NULL
  colnames(f_edges) <- c("from", "to")
  f_edges <- f_edges[f_edges$to != 0,]

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
  for (i in 1:length(amounts)) {
    from <- nodes[rollbacks[i],]$nodes
    graph <- DiagrammeR::select_nodes_by_id(graph, from)
    if (amounts[i]) for (j in 1:amounts[i]) {
      graph <- DiagrammeR::trav_in(graph)
    }
    to <- as.numeric(DiagrammeR::get_selection(graph))
    graph <- DiagrammeR::clear_selection(graph)
    r_edges <- rbind(r_edges, data.frame(from=from, to=to))
  }

  # compose edges
  edges <- unique(rbind(f_edges, b_edges, r_edges))
  edges$color <- "black"
  edges[c(forks),]$color <- "gray"
  edges$style <- "solid"
  edges[c(forks),]$style <- "dashed"

  graph <- DiagrammeR::create_graph(nodes, edges)
  DiagrammeR::render_graph(graph, ...)
}
