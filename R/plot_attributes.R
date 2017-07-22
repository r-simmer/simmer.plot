plot_attributes <- function(x, metric=NULL, keys=NULL, ...) {
  monitor_data <- get_mon_attributes(x)
  if (length(keys) > 0)
    monitor_data <- monitor_data %>%
      dplyr::filter(.data$key %in% keys)
  if (nrow(monitor_data) == 0)
    stop("no data available for the 'keys' provided")

  plot_obj <-
    ggplot(monitor_data) +
    aes_(x = ~time, y = ~value) +
    geom_step(aes_(group = ~replication), alpha = set_alpha(monitor_data)) +
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
