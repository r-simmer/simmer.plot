plot_attributes <- function(x, metric=NULL, keys=c(), ...) {
  monitor_data <- get_mon_attributes(x)
  if (length(keys) > 0)
    monitor_data <- monitor_data %>%
      dplyr::filter_(~key %in% keys)
  if (nrow(monitor_data) == 0)
    stop("no data available")

  plot_obj <-
    ggplot(monitor_data) +
    aes_(x = ~time, y = ~value) +
    geom_step(alpha = .4, aes_(group = ~replication)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("value") +
    expand_limits(y = 0)

  if (length(unique(monitor_data$key)) > 1) {
    plot_obj <- plot_obj +
      ggtitle("Attribute evolution") +
      facet_wrap(~key, scales = "free_y")
  } else {
    plot_obj <- plot_obj +
      ggtitle(paste0("Attribute evolution: ", monitor_data$key[[1]]))
  }

  plot_obj

}
