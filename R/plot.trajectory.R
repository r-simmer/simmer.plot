#' Plot Method for \code{trajectory} Objects
#'
#' A method for the \code{\link{plot}} generic that plots a diagram of the given trajectory.
#'
#' @param x a simmer trajectory.
#' @param engine a string specifying a layout engine (see \code{\link[DiagrammeR]{grViz}}).
#' @param fill discrete color palette for resource identification.
#' @param verbose show additional info directly in the labels.
#' @param ... additional parameters for \code{\link[DiagrammeR]{render_graph}}.
#'
#' @return Returns an \code{htmlwidget}.
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

  # this should be done by DiagrammeR
  # see https://github.com/r-simmer/simmer.plot/issues/19
  if (exists(".Random.seed", where=globalenv())) {
    currentSeed <- get(".Random.seed", pos=globalenv())
    on.exit(assign(".Random.seed", currentSeed, pos=globalenv()))
  } else on.exit(rm(".Random.seed", pos=globalenv()))

  trajectory_graph(x, fill, verbose) %>%
    DiagrammeR::add_global_graph_attrs("layout", engine, "graph") %>%
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
  ids <- sub(" +->.*", "", sub(".*<- +", "", out))
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

  # tags
  out <- sub(".* -> .* +\\| ", "", out)
  tags <- sub(" .*", "", out)
  tags <- replace(tags, !grepl("^\\[.*\\]$", tags), NA)
  nodes$label <- ifelse(!is.na(tags), paste(nodes$label, tags), nodes$label)
  nodes$tag <- gsub("\\[|\\]", "", tags)

  # additional info & rollbacks & resources
  out <- sub("^\\[.*\\] ", "", out)
  info <- sub(" \\}", "", out)
  info[rollbacks] <- if (utils::packageVersion("simmer") > "4.4.5")
    sub("target: ", "", info[rollbacks]) else sub("amount: ", "", info[rollbacks])
  targets <- sub("( \\(.*\\))?,.*", "", info[rollbacks])
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
  nodes$fillcolor <- nodes$color
  nodes$fontcolor <- "black"

  # resolve rollbacks from back connections
  r_edges <- NULL
  graph <- DiagrammeR::create_graph(nodes, b_edges)
  suppressMessages({for (i in seq_along(targets)) {
    from <- nodes[rollbacks[i],]$id
    graph <- DiagrammeR::select_nodes_by_id(graph, from)
    indeg <- DiagrammeR::get_degree_in(graph)
    to <- as.numeric(DiagrammeR::get_selection(graph))
    steps <- tryCatch(as.numeric(targets[i]), warning=function(e) targets[i])
    try({
      found <- steps == 0
      while (!found && indeg[indeg$id==to,]$indegree) {
        graph <- DiagrammeR::trav_in(graph)
        to <- as.numeric(DiagrammeR::get_selection(graph))
        if (is.numeric(steps)) {
          steps <- steps - 1
          found <- steps == 0
        } else {
          tag <- DiagrammeR::get_node_attrs(graph, "tag", to)
          found <- isTRUE(tag == steps)
        }
      }
    }, silent = TRUE)
    graph <- DiagrammeR::clear_selection(graph)
    r_edges <- rbind(r_edges, data.frame(from=from, to=to))
  }})

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
    if (grepl("function", clones[i,]$tooltip)) next
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
