#' @name get_mon
#' @export
get_mon_arrivals <- function(...) {
  x <- simmer::get_mon_arrivals(...)
  class(x) <- c("arrivals", class(x))
  x
}

#' @name plot.mon
#'
#' @details The S3 method for 'arrivals' provides three metrics: \code{"activity_time"},
#' \code{"waiting_time"}, and \code{"flow_time"}. The \code{"activity_time"} is
#' the amount of time spent in active state (i.e., in \code{timeout} activities),
#' and it is already provided in the output of \code{get_mon_arrivals}. The
#' \code{"flow_time"} is the amount of time spent in the system, and it is
#' computed as follows: \code{flow = end_time - start_time}. Finally, the
#' \code{"waiting_time"} is the amount of time spent waiting (e.g., in resources'
#' queues, or due to a \code{wait} activity...), and it is computed as follows:
#' \code{waiting_time = flow_time - activity_time}. This method does not apply
#' any summary, but just shows a line plot of the values throughout the simulation.
#'
#' @export
plot.arrivals <- function(x, metric=c("activity_time", "waiting_time", "flow_time"), ...) {
  metric <- match.arg(metric)

  if (nrow(x) == 0)
    stop("no data available")

  x <- x %>%
    dplyr::mutate(flow_time = .data$end_time - .data$start_time,
                  waiting_time = .data$flow_time - .data$activity_time)

  dispatch_next(metric, x)
}

plot.arrivals.activity_time <- function(x) {
  ggplot(x) +
    aes_(x = ~end_time, y = ~activity_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(x)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("activity time") +
    ggtitle("Activity time evolution") +
    expand_limits(y = 0)
}

plot.arrivals.waiting_time <- function(x) {
  ggplot(x) +
    aes_(x = ~end_time, y = ~waiting_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(x)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("waiting time") +
    ggtitle("Waiting time evolution") +
    expand_limits(y = 0)
}

plot.arrivals.flow_time <- function(x) {
  ggplot(x) +
    aes_(x = ~end_time, y = ~flow_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(x)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("flow time") +
    ggtitle("Flow time evolution") +
    expand_limits(y = 0)
}
