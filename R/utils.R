#' @export
plot.list <- function(x, ...) {
  if (length(class(x)) == 1) {
    stopifnot(all(class(x[[1]]) == sapply(x, class)))
    plot_list_proxy(x, ...)
  } else NextMethod()
}

plot_list_proxy <- function(x, ...) {
  if (all(sapply(x, inherits, class(x[[1]]))))
    class(x) <- c(class(x), class(x[[1]]))
  plot(x, ...)
}

dispatch_next <- function(.next, ...) {
  caller <- match.call(sys.function(-1), sys.call(-1))
  caller <- as.character(caller)[[1]]
  caller <- strsplit(caller, ".", fixed = TRUE)[[1]][[1]]
  do.call(paste0(caller, "_", .next), list(...))
}

set_alpha <- function(data) {
  1.0 / (log(max(data$replication)) + 1)
}
