#' Plot method for trajectory objects
#'
#' A method for the \code{\link{plot}} generic that plots a diagram of the given trajectory.
#'
#' @param x a simmer trajectory.
#' @param engine a string specifying a layout engine (see \code{\link{grViz}}).
#' @param fill discrete color palette for resource identification.
#' @param verbose show additional info directly in the labels.
#' @param ... additional parameters for \code{\link{render_graph}}.
#'
#' @return Returns an \code{htmlwidget}.
#' @importFrom graphics plot
#' @export
#'
#' @examples
#' x <- trajectory() %>%
#'   seize("res", 1) %>%
#'   timeout(1) %>%
#'   release("res", 1) %>%
#'   rollback(3)
#'
#' plot(x)
plot.trajectory <- function(x, engine="dot", fill=scales::brewer_pal("qual"), verbose=FALSE, ...) {
  stopifnot(length(x) > 0)

  trajectory_graph(x, fill, verbose) %>%
    DiagrammeR::set_global_graph_attrs("layout", engine, "graph") %>%
    DiagrammeR::add_global_graph_attrs("fontname", "sans-serif", "node") %>%
    DiagrammeR::add_global_graph_attrs("width", 1.5, "node") %>%
    DiagrammeR::render_graph(...)
}

trajectory_graph <- function(x, fill, verbose=FALSE) {
  # capture output with pointers
  old_verbose <- x$verbose
  x$verbose <- TRUE
  out <- gsub("\b", "", utils::capture.output(x))
  x$verbose <- old_verbose
  out <- paste0(out, "\n")
  out <- gsub("}", "}\n", out, fixed=TRUE)
  out <- utils::capture.output(cat(out))
  out <- out[grep("0x", out, fixed=TRUE)]
  if (!length(out))
    stop("no activity pointers found! \n",                                                      # nocov
         "  This is embarrassing... The trajectory cannot be plotted without pointers!\n",      # nocov
         "  Please, consider filling a bug at https://github.com/r-simmer/simmer.plot/issues")  # nocov

  # assign reproducible identifiers
  out <- gsub("0x0* ", "0 ", out)
  ids <- sub(" ->.*", "", sub(".*<- ", "", out))
  for (i in seq_along(ids)) out <- gsub(ids[i], i, out)

  # find forks & rollbacks & seizes/releases
  level <- nchar(sub("\\{.*", "", out)) / 2
  forks <- which(diff(level) == 1)
  rollbacks <- grep("Activity: Rollback", out, fixed=TRUE)
  seizes <- grep("Activity: Seize", out, fixed=TRUE)
  releases <- grep("Activity: Release", out, fixed=TRUE)

  # find activity names
  out <- sub(".*Activity: ", "", out)
  nodes <- data.frame(label = sub(" .*", "", out), stringsAsFactors=FALSE)
  nodes$type <- nodes$label
  nodes$shape <- "box"
  nodes$shape[c(forks, rollbacks)] <- "diamond"
  nodes$style <- "solid"
  nodes$style[c(forks, rollbacks)] <- "filled"
  nodes$style[c(seizes, releases)] <- "filled"
  nodes$color <- "black"
  nodes$color[c(forks, rollbacks)] <- "lightgrey"

  # back connections
  out <- sub("[[:alpha:]]*[[:space:]]*\\| ", "", out)
  b_edges <- suppressWarnings(
    sub(" ->.*", "", out) %>%
      strsplit(" <- ", fixed=TRUE) %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame(stringsAsFactors=FALSE)
  )
  rownames(b_edges) <- NULL
  colnames(b_edges) <- c("from", "to")
  nodes$id <- b_edges$to
  b_edges <- utils::tail(b_edges, -1)

  # forward connections
  out <- sub(".*<- ", "", out)
  f_edges <- suppressWarnings(
    sub(" \\|.*", "", out) %>%
      strsplit(" -> ", fixed=TRUE) %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame(stringsAsFactors=FALSE)
  )
  rownames(f_edges) <- NULL
  colnames(f_edges) <- c("from", "to")
  f_edges <- f_edges[f_edges$to != 0 & !is.na(f_edges$to),]

  # additional info & rollbacks & resources
  out <- sub(".* -> .* +\\| ", "", out)
  info <- sub(" \\}", "", out)
  info[rollbacks] <- sub("amount: ", "", info[rollbacks])
  amounts <- as.numeric(sub(" \\(.*", "", info[rollbacks]))
  info[rollbacks] <- sub(".*, ", "", info[rollbacks])
  resources <- sub("resource: ", "", info[c(seizes, releases)])
  resources <- sub(",* .*", "", resources)
  nodes$tooltip <- info
  if (verbose)
    nodes$label <- paste0(nodes$label, "\n", gsub("\\||,", "\n", info))
  nodes$color[c(seizes, releases)] <- dplyr::left_join(
      data.frame(name = resources, stringsAsFactors = FALSE),
      data.frame(name = unique(resources),
                 color = fill(length(unique(resources))),
                 stringsAsFactors = FALSE),
      by = "name")$color

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
    for (i in forks) {
      edges$color[which(edges$from == i)[-1]] <- "grey"
      edges$style[which(edges$from == i)[-1]] <- "dashed"
    }
    for (i in rollbacks) {
      edges$color[rev(which(edges$from == i))[1]] <- "grey"
      edges$style[rev(which(edges$from == i))[1]] <- "dashed"
    }
  } else edges <- NULL

  DiagrammeR::create_graph(nodes, edges) %>%
    postprocess_clones()
}

postprocess_clones <- function(graph) {
  clones <- dplyr::filter(graph$nodes_df, .data$type == "Clone")
  for (i in seq_len(nrow(clones))) {
    n <- as.numeric(strsplit(clones[i,]$tooltip, "n: ", fixed=TRUE)[[1]][2])
    id_clone <- clones[i,]$id
    edges <- dplyr::filter(graph$edges_df, .data$from == id_clone)
    if (n+1 <= nrow(edges))
      graph <- DiagrammeR::delete_edge(graph, id=edges[1,]$id)
    edges <- dplyr::filter(graph$edges_df, .data$from == id_clone)
    while (n < nrow(edges)) {
      graph <- DiagrammeR::delete_edge(graph, id=edges[nrow(edges),]$id)
      edges <- dplyr::filter(graph$edges_df, .data$from == id_clone)
    }
  }
  graph
}
