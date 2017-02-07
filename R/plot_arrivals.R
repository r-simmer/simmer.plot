plot_arrivals <- function(x, metric=c("activity_time", "waiting_time", "flow_time"), ...) {
  metric <- match.arg(metric)

  monitor_data <- get_mon_arrivals(x)
  if (nrow(monitor_data) == 0)
    stop("no data available")

  monitor_data <- monitor_data %>%
    dplyr::mutate_(flow_time = ~end_time - start_time,
                   waiting_time = ~flow_time - activity_time)

  dispatch_next(metric, monitor_data, ...)
}

plot_arrivals_activity_time <- function(monitor_data) {
  ggplot(monitor_data) +
    aes_(x = ~end_time, y = ~activity_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(monitor_data)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("activity time") +
    ggtitle("Activity time evolution") +
    expand_limits(y = 0)
}

plot_arrivals_waiting_time <- function(monitor_data) {
  ggplot(monitor_data) +
    aes_(x = ~end_time, y = ~waiting_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(monitor_data)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("waiting time") +
    ggtitle("Waiting time evolution") +
    expand_limits(y = 0)
}

plot_arrivals_flow_time <- function(monitor_data) {
  ggplot(monitor_data) +
    aes_(x = ~end_time, y = ~flow_time) +
    geom_line(aes_(group = ~replication), alpha = set_alpha(monitor_data)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("flow time") +
    ggtitle("Flow time evolution") +
    expand_limits(y = 0)
}
