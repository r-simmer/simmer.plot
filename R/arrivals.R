#' Plot evolution of arrival times
#'
#' Plot the evolution of arrival related times (flow, activity and waiting time).
#'
#' @inheritParams plot_resource_usage
#' @param type one of \code{c("activity_time", "waiting_time", "flow_time")}.
#'
#' @return Returns a ggplot2 object.
#' @seealso \code{\link{plot_resource_usage}}, \code{\link{plot_resource_utilization}},
#' \code{\link{plot_attributes}}.
#' @export
plot_evolution_arrival_times <- function(envs, type=c("activity_time", "waiting_time", "flow_time")){
  # Hack to avoid spurious notes
  end_time <- start_time <- flow_time <- activity_time <-
    replication <- waiting_time <- NULL

  type <- match.arg(type)

  monitor_data <- envs %>% simmer::get_mon_arrivals() %>%
    dplyr::mutate(flow_time = end_time - start_time,
                  waiting_time = flow_time - activity_time)

  if (type == "flow_time") {
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x = end_time, y = flow_time) +
      ggplot2::geom_line(alpha = .4, ggplot2::aes(group = replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("flow time") +
      ggplot2::ggtitle("Flow time evolution") +
      ggplot2::expand_limits(y = 0)
  } else if (type == "waiting_time") {
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x = end_time, y = waiting_time) +
      ggplot2::geom_line(alpha = .4, ggplot2::aes(group = replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("waiting time") +
      ggplot2::ggtitle("Waiting time evolution") +
      ggplot2::expand_limits(y = 0)
  } else if (type == "activity_time") {
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x = end_time, y = activity_time) +
      ggplot2::geom_line(alpha = .4, ggplot2::aes(group = replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("activity time") +
      ggplot2::ggtitle("Activity time evolution") +
      ggplot2::expand_limits(y = 0)
  }
}
