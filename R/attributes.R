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
  # Hack to avoid spurious notes
  time <- key <- value <- replication <- NULL

  monitor_data <- envs %>% simmer::get_mon_attributes()

  if (length(keys) > 0) monitor_data <- monitor_data %>% dplyr::filter(key %in% keys)

  plot_obj <-
    ggplot2::ggplot(monitor_data) +
    ggplot2::aes(x = time, y = value) +
    ggplot2::geom_step(alpha = .4, ggplot2::aes(group = replication)) +
    ggplot2::stat_smooth() +
    ggplot2::xlab("simulation time") +
    ggplot2::ylab("value") +
    ggplot2::expand_limits(y = 0)

  if (length(unique(monitor_data$key)) > 1) {
    plot_obj <- plot_obj +
      ggplot2::ggtitle("Attribute evolution") +
      ggplot2::facet_wrap(~key, scales = "free_y")
  } else {
    plot_obj <- plot_obj +
      ggplot2::ggtitle(paste0("Attribute evolution: ", monitor_data$key[[1]]))
  }

  plot_obj

}
