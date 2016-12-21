#' Plot method for simmer objects
#'
#' A method for the \code{\link{plot}} generic.
#'
#' @param x a single simmer environment or a list of environments representing several replications.
#' @param what type of plot: arrivals, attributes or resources.
#' @param metric specific metric. See \code{\link{plot_resources}}, \code{\link{plot_arrivals}} and
#' \code{\link{plot_attributes}} for more details.
#' @param ... further arguments for each kind of plot.
#'
#' @import simmer ggplot2
#' @importFrom graphics plot
#' @export
plot.simmer <- function(x, what=c("resources", "arrivals", "attributes"), metric=NULL, ...) {
  what <- match.arg(what)
  dispatch_next(what, x, metric, ...)
}

dispatch_next <- function(.next, ...) {
  caller <- match.call(sys.function(-1), sys.call(-1))
  caller <- as.character(caller)[[1]]
  caller <- strsplit(caller, ".", fixed = TRUE)[[1]][[1]]
  do.call(paste0(caller, "_", .next), list(...))
}
