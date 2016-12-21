#' Plot evolution of attribute data
#'
#' Plot the evolution of user-supplied attribute data.
#'
#' @inheritParams plot_resource_usage
#' @param keys the keys of attributes you want to plot (if left empty, all attributes are shown).
#'
#' @return Returns a ggplot2 object.
#' @seealso \code{\link{plot_resource_usage}}, \code{\link{plot_resource_utilization}},
#' \code{\link{plot_evolution_arrival_times}}.
#' @export
plot_attributes <- function(envs, keys=c()) {
  monitor_data <- envs %>% get_mon_attributes()

  if (length(keys) > 0)
    monitor_data <- monitor_data %>%
      dplyr::filter_(~key %in% keys)

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
