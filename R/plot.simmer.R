#' Plot Method for \code{simmer} Objects
#'
#' Deprecated. See \code{\link{plot.mon}} instead.
#'
#' @param x a single simmer environment or a list of environments representing several replications.
#' @param what type of plot, one of \code{c("resources", "arrivals", "attributes")}.
#' @param metric specific metric for each type of plot.
#' \describe{
#'   \item{\code{what = "resources"}}{one of \code{c("usage", "utilization")}.}
#'   \item{\code{what = "arrivals"}}{one of \code{c("activity_time", "waiting_time", "flow_time")}.}
#'   \item{\code{what = "attributes"}}{no metrics at the moment.}
#' }
#' @param ... further arguments for each kind of plot.
#' \describe{
#'   \item{\code{what = "resources"}}{\describe{
#'     \item{all metrics}{\describe{
#'       \item{\code{names}}{the name of the resource(s) (a single string or a character
#'       vector) to show.}
#'     }}
#'     \item{\code{metric = "usage"}}{\describe{
#'       \item{\code{items}}{the components of the resource to be plotted, one or more
#'       of \code{c("system", "queue", "server")}.}
#'       \item{\code{steps}}{if \code{TRUE}, shows the instantaneous usage instead
#'       of the cumulative average.}
#'     }}
#'   }}
#'   \item{\code{what = "attributes"}}{\describe{
#'     \item{keys}{the keys of attributes you want to plot (if left empty, all attributes are shown).}
#'   }}
#' }
#'
#' @return Returns a ggplot2 object.
#' @export
#'
plot.simmer <- function(x, what=c("resources", "arrivals", "attributes"), metric=NULL, ...) {
  what <- match.arg(what)
  .Deprecated(paste0("plot(get_mon_", what, "(x))"))
  x <- do.call(paste0("get_mon_", what), list(.envs=x))
  plot(x, metric, ...)
}

#' @export
plot.wrap <- function(x, ...) plot.simmer(x, ...)
