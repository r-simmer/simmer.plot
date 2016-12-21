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
  type <- match.arg(type)

  monitor_data <- envs %>% get_mon_arrivals() %>%
    dplyr::mutate_(flow_time = ~end_time - start_time,
                   waiting_time = ~flow_time - activity_time)

  if (type == "flow_time") {
    ggplot(monitor_data) +
      aes_(x = ~end_time, y = ~flow_time) +
      geom_line(alpha = .4, aes_(group = ~replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("flow time") +
      ggtitle("Flow time evolution") +
      expand_limits(y = 0)
  } else if (type == "waiting_time") {
    ggplot(monitor_data) +
      aes_(x = ~end_time, y = ~waiting_time) +
      geom_line(alpha = .4, aes_(group = ~replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("waiting time") +
      ggtitle("Waiting time evolution") +
      expand_limits(y = 0)
  } else if (type == "activity_time") {
    ggplot(monitor_data) +
      aes_(x = ~end_time, y = ~activity_time) +
      geom_line(alpha = .4, aes_(group = ~replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("activity time") +
      ggtitle("Activity time evolution") +
      expand_limits(y = 0)
  }
}
