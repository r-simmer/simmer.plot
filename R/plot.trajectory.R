#' @export
plot.trajectory <- function(x, ...) {
  old_verbose <- x$verbose
  x$verbose <- TRUE
  out <- utils::capture.output(x)
  x$verbose <- old_verbose
  out <- out[grep("0x", out)]

  #level <- nchar(sub("\\{.*", "", out)) / 2
  out <- sub(".*Activity: ", "", out)
  nodes <- as.data.frame(sub(" .*", "", out), stringsAsFactors=FALSE)
  colnames(nodes) <- "label"
  nodes$shape <- "box"

  out <- sub("[[:alpha:]]*[[:space:]]*\\| ", "", out)
  b_edges <- sub(" ->.*", "", out)
  b_edges <- t(as.data.frame(strsplit(b_edges, " <- ")))
  b_edges <- as.data.frame(b_edges, stringsAsFactors=FALSE)
  colnames(b_edges) <- c("from", "to")
  nodes$nodes <- b_edges$to
  b_edges <- utils::tail(b_edges, -1)
  rownames(b_edges) <- NULL

  out <- sub(".*<- ", "", out)
  f_edges <- sub(" \\|.*", "", out)
  f_edges <- t(as.data.frame(strsplit(f_edges, " -> ")))
  f_edges <- as.data.frame(f_edges, stringsAsFactors=FALSE)
  rownames(f_edges) <- NULL
  colnames(f_edges) <- c("from", "to")
  f_edges <- f_edges[-grep("0 +", f_edges$to),]

  edges <- unique(rbind(f_edges, b_edges))
  edges$color <- "black"
  #edges$dashes <- FALSE
  #edges[dashes,]$dashes <- TRUE

  graph <- DiagrammeR::create_graph(nodes, edges)
  DiagrammeR::render_graph(graph, ...)
}
