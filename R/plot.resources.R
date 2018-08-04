#' @name get_mon
#' @export
get_mon_resources <- function(...) {
  x <- simmer::get_mon_resources(...)
  class(x) <- c("resources", class(x))
  x
}

#' @name plot.mon
#' @param names resources to plot (if left empty, all resources are shown).
#' @param ... further arguments
#' \describe{
#'   \item{\code{items}, for \code{plot.resources(metric="usage")}}{components of
#'   the resource to plot, one or more of \code{c("system", "queue", "server")}.}
#'   \item{\code{steps}, for \code{plot.resources(metric="usage")}}{if \code{TRUE},
#'   shows the instantaneous usage instead of the cumulative average.}
#' }
#'
#' @details The S3 method for 'resources' provides two metrics: \code{"usage"}
#' and \code{"utilization"}. The \code{"usage"} metric shows a line graph of
#' the cumulative average resource usage throughout the simulation, for each
#' resource, replication and item (by default, queue, server and system, which
#' is the sum of queue and server). If \code{steps=TRUE}, a stairstep graph with
#' the instantaneous values is provided instead. The \code{"utilization"} metric
#' shows a bar plot of the average resource utilization (total time in use
#' divided by the total simulation time). For multiple replications, the bar
#' represents the median, and the error bars represent the quartiles. Thus, if
#' a single replication is provided, the bar and the error bar coincide.
#'
#' @export
plot.resources <- function(x, metric=c("usage", "utilization"), names, ...) {
  metric <- match.arg(metric)

  if (!missing(names)) x <- x %>%
    dplyr::filter(.data$resource %in% names) %>%
    dplyr::mutate(resource = factor(.data$resource, levels = names))

  if (nrow(x) == 0)
    stop("no data available or resource 'names' not found")

  dispatch_next(metric, x, ...)
}

plot.resources.usage <- function(x, items=c("queue", "server", "system"), steps=FALSE) {
  items <- match.arg(items, several.ok = TRUE)

  limits <- x %>%
    dplyr::mutate(server = .data$capacity, queue = .data$queue_size, system = .data$limit) %>%
    tidyr::gather("item", "value", c("queue", "server", "system")) %>%
    dplyr::filter(.data$item %in% items) %>%
    dplyr::mutate(item = factor(.data$item, levels = items))

  x <- x %>%
    tidyr::gather("item", "value", c("queue", "server", "system")) %>%
    dplyr::filter(.data$item %in% items) %>%
    dplyr::mutate(item = factor(.data$item, levels = items)) %>%
    dplyr::group_by(.data$resource, .data$replication, .data$item) %>%
    dplyr::mutate(mean = c(0, cumsum(utils::head(.data$value, -1) * diff(.data$time))) / .data$time) %>%
    dplyr::ungroup()

  plot_obj <-
    ggplot(x, aes_(x = ~time, color = ~item)) +
    facet_grid(~resource) +
    geom_step(aes_(y = ~value, group = ~interaction(replication, item)), limits, lty = 2) +
    ggtitle(paste("Resource usage")) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y = 0)

  if (steps == TRUE)
    plot_obj <- plot_obj +
      geom_step(aes_(y = ~value, group = ~interaction(replication, item)), alpha = set_alpha(x))
  else
    plot_obj <- plot_obj +
      geom_line(aes_(y = ~mean, group = ~interaction(replication, item)), alpha = set_alpha(x))

  plot_obj
}

plot.resources.utilization <- function(x) {
  x <- x %>%
    dplyr::group_by(.data$resource, .data$replication) %>%
    dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    dplyr::mutate(in_use = .data$dt * dplyr::lag(.data$server / .data$capacity)) %>%
    dplyr::summarise(utilization = sum(.data$in_use, na.rm = TRUE) / sum(.data$dt, na.rm=TRUE)) %>%
    dplyr::summarise(Q25 = stats::quantile(.data$utilization, .25),
                     Q50 = stats::quantile(.data$utilization, .5),
                     Q75 = stats::quantile(.data$utilization, .75))

  ggplot(x) +
    aes_(x = ~resource, y = ~Q50, ymin = ~Q25, ymax = ~Q75) +
    geom_bar(stat = "identity") +
    geom_errorbar(width = .25, color = "black") +
    ggtitle("Resource utilization") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 2, .2)) +
    ylab("utilization")
}
