#' Plot usage of a resource over time
#'
#' Plot the usage of a resource over the simulation time frame.
#'
#' @param envs a single simmer environment or a list of environments representing several replications.
#' @param resource_name the name of the resource (character value).
#' @param items the components of the resource to be plotted.
#' @param steps adds the changes in the resource usage.
#'
#' @return Returns a ggplot2 object.
#' @seealso \code{\link{plot_resource_utilization}},
#' \code{\link{plot_evolution_arrival_times}}, \code{\link{plot_attributes}}.
#' @export
plot_resource_usage <- function(envs, resource_name, items=c("system", "queue", "server"), steps = FALSE) {
  # Hack to avoid spurious notes
  resource <- item <- value <- server <- queue <- system <- replication <- time <- NULL

  items <- match.arg(items, several.ok = TRUE)

  limits <- envs %>% simmer::get_mon_resources(data = "limits") %>%
    dplyr::filter(resource == resource_name) %>%
    tidyr::gather(item, value, server, queue, system) %>%
    dplyr::mutate(item = factor(item)) %>%
    dplyr::filter(item %in% items)

  monitor_data <- envs %>% simmer::get_mon_resources(data = "counts") %>%
    dplyr::filter(resource == resource_name) %>%
    tidyr::gather(item, value, server, queue, system) %>%
    dplyr::mutate(item = factor(item)) %>%
    dplyr::filter(item %in% items) %>%
    dplyr::group_by(resource, replication, item) %>%
    dplyr::mutate(mean = c(0, cumsum(utils::head(value, -1) * diff(time))) / time) %>%
    dplyr::ungroup()

  if (is.list(envs)) env <- envs[[1]]
  else env <- envs

  plot_obj <-
    ggplot2::ggplot(monitor_data) +
    ggplot2::aes(x = time, color = item) +
    ggplot2::geom_line(ggplot2::aes(y = mean, group = interaction(replication, item))) +
    ggplot2::geom_step(ggplot2::aes(y = value, group = interaction(replication, item)), limits, lty = 2) +
    ggplot2::ggtitle(paste("Resource usage:", resource_name)) +
    ggplot2::ylab("in use") +
    ggplot2::xlab("time") +
    ggplot2::expand_limits(y = 0)

  if (steps == T) {
    plot_obj <- plot_obj +
      ggplot2::geom_step(ggplot2::aes(y = value, group = interaction(replication, item)), alpha = .4)
  }

  plot_obj
}

#' Plot utilization of resources
#'
#' Plot the utilization of specified resources in the simulation.
#'
#' @inheritParams plot_resource_usage
#' @param resources a character vector with at least one resource specified - e.g. "c('res1','res2')".
#'
#' @return Returns a ggplot2 object.
#' @seealso \code{\link{plot_resource_usage}},
#' \code{\link{plot_evolution_arrival_times}}, \code{\link{plot_attributes}}.
#' @export
plot_resource_utilization <- function(envs, resources) {
  # Hack to avoid spurious notes
  resource <- item <- value <- server <- queue <- system <- replication <- capacity <- runtime <-
    in_use <- utilization <- Q25 <- Q50 <- Q75 <- time <- NULL

  if (is.list(envs)) env <- envs[[1]]
  else env <- envs

  monitor_data <- envs %>% simmer::get_mon_resources(data = "counts") %>%
    dplyr::filter(resource %in% resources) %>%
    tidyr::gather(item, value, server, queue, system) %>%
    dplyr::mutate(item = factor(item)) %>%
    dplyr::filter(item == "server") %>%
    dplyr::group_by(resource) %>%
    dplyr::mutate(capacity = simmer::get_capacity(env, resource[[1]])) %>%
    dplyr::group_by(replication) %>%
    dplyr::mutate(runtime = max(time)) %>%
    dplyr::group_by(resource, replication, capacity, runtime) %>%
    dplyr::mutate(in_use = (time - dplyr::lag(time)) * dplyr::lag(value)) %>%
    dplyr::group_by(resource, replication, capacity, runtime) %>%
    dplyr::summarise(in_use = sum(in_use, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(utilization = in_use / capacity / runtime) %>%
    dplyr::group_by(resource, capacity) %>%
    dplyr::summarise(Q25 = stats::quantile(utilization, .25),
                     Q50 = stats::quantile(utilization, .5),
                     Q75 = stats::quantile(utilization, .75))

  ggplot2::ggplot(monitor_data) +
    ggplot2::aes(x = resource, y = Q50, ymin = Q25, ymax = Q75) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_errorbar(width = .25, color = "black") +
    ggplot2::ggtitle("Resource utilization") +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 2, .2)) +
    ggplot2::ylab("utilization")
}
