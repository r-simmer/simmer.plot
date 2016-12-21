plot_resources <- function(x, metric=c("usage", "utilization"), names, ...) {
  metric <- match.arg(metric)

  monitor_data <- get_mon_resources(x, data = c("counts", "limits")) %>%
    dplyr::filter_(~resource %in% names)
  if (nrow(monitor_data) == 0)
    stop("no data available")

  dispatch_next(metric, monitor_data, ...)
}

plot_resources_usage <- function(monitor_data, items=c("system", "queue", "server"), steps=FALSE) {
  items <- match.arg(items, several.ok = TRUE)

  limits <- monitor_data %>%
    dplyr::mutate_(server = ~capacity, queue = ~queue_size, system = ~limit) %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item %in% items)

  monitor_data <- monitor_data %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item %in% items) %>%
    dplyr::group_by_(~resource, ~replication, ~item) %>%
    dplyr::mutate_(mean = ~c(0, cumsum(utils::head(value, -1) * diff(time))) / time) %>%
    dplyr::ungroup()

  plot_obj <-
    ggplot(monitor_data, aes_(x = ~time, color = ~item)) +
    facet_grid(~resource) +
    geom_line(aes_(y = ~mean, group = ~interaction(replication, item))) +
    geom_step(aes_(y = ~value, group = ~interaction(replication, item)), limits, lty = 2) +
    ggtitle(paste("Resource usage")) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y = 0)

  if (steps == T) {
    plot_obj <- plot_obj +
      geom_step(aes_(y = ~value, group = ~interaction(replication, item)), alpha = .4)
  }

  plot_obj
}

plot_resources_utilization <- function(monitor_data) {
  monitor_data <- monitor_data %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item == "server") %>%
    dplyr::group_by_(~resource) %>%
    dplyr::group_by_(~replication) %>%
    dplyr::mutate_(runtime = "max(time)") %>%
    dplyr::group_by_(~resource, ~replication, ~capacity, ~runtime) %>%
    dplyr::mutate_(in_use = ~(time - dplyr::lag(time)) * dplyr::lag(value)) %>%
    dplyr::group_by_(~resource, ~replication, ~capacity, ~runtime) %>%
    dplyr::summarise_(in_use = ~sum(in_use, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(utilization = ~in_use / capacity / runtime) %>%
    dplyr::group_by_(~resource, ~capacity) %>%
    dplyr::summarise_(Q25 = ~stats::quantile(utilization, .25),
                      Q50 = ~stats::quantile(utilization, .5),
                      Q75 = ~stats::quantile(utilization, .75))

  ggplot(monitor_data) +
    aes_(x = ~resource, y = ~Q50, ymin = ~Q25, ymax = ~Q75) +
    geom_bar(stat = "identity") +
    geom_errorbar(width = .25, color = "black") +
    ggtitle("Resource utilization") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 2, .2)) +
    ylab("utilization")
}
