plot_resources <- function(x, metric=c("usage", "utilization"), names=NULL, ...) {
  metric <- match.arg(metric)

  monitor_data <- get_mon_resources(x, data = c("counts", "limits")) %>%
    dplyr::filter(.data$resource %in% names)
  if (nrow(monitor_data) == 0)
    stop("no data available for the 'names' provided")

  dispatch_next(metric, monitor_data, ...)
}

plot_resources_usage <- function(monitor_data, items=c("system", "queue", "server"), steps=FALSE) {
  items <- match.arg(items, several.ok = TRUE)

  limits <- monitor_data %>%
    dplyr::mutate(server = .data$capacity, queue = .data$queue_size, system = .data$limit) %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate(item = factor(.data$item)) %>%
    dplyr::filter(.data$item %in% items)

  monitor_data <- monitor_data %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate(item = factor(.data$item)) %>%
    dplyr::filter(.data$item %in% items) %>%
    dplyr::group_by(.data$resource, .data$replication, .data$item) %>%
    dplyr::mutate(mean = c(0, cumsum(utils::head(.data$value, -1) * diff(.data$time))) / .data$time) %>%
    dplyr::ungroup()

  plot_obj <-
    ggplot(monitor_data, aes_(x = ~time, color = ~item)) +
    facet_grid(~resource) +
    geom_step(aes_(y = ~value, group = ~interaction(replication, item)), limits, lty = 2) +
    ggtitle(paste("Resource usage")) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y = 0)

  if (steps == TRUE)
    plot_obj <- plot_obj +
      geom_step(aes_(y = ~value, group = ~interaction(replication, item)), alpha = set_alpha(monitor_data))
  else
    plot_obj <- plot_obj +
      geom_line(aes_(y = ~mean, group = ~interaction(replication, item)), alpha = set_alpha(monitor_data))

  plot_obj
}

plot_resources_utilization <- function(monitor_data) {
  monitor_data <- monitor_data %>%
    dplyr::group_by(.data$resource, .data$replication) %>%
    dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    dplyr::mutate(in_use = .data$dt * dplyr::lag(.data$server / .data$capacity)) %>%
    dplyr::summarise(utilization = sum(.data$in_use, na.rm = TRUE) / sum(.data$dt, na.rm=TRUE)) %>%
    dplyr::summarise(Q25 = stats::quantile(.data$utilization, .25),
                     Q50 = stats::quantile(.data$utilization, .5),
                     Q75 = stats::quantile(.data$utilization, .75))

  ggplot(monitor_data) +
    aes_(x = ~resource, y = ~Q50, ymin = ~Q25, ymax = ~Q75) +
    geom_bar(stat = "identity") +
    geom_errorbar(width = .25, color = "black") +
    ggtitle("Resource utilization") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 2, .2)) +
    ylab("utilization")
}
