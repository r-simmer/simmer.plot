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
  items <- match.arg(items, several.ok = TRUE)

  limits <- envs %>% get_mon_resources(data = "limits") %>%
    dplyr::filter_(~resource == resource_name) %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item %in% items)

  monitor_data <- envs %>% get_mon_resources(data = "counts") %>%
    dplyr::filter_(~resource == resource_name) %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item %in% items) %>%
    dplyr::group_by_(~resource, ~replication, ~item) %>%
    dplyr::mutate_(mean = ~c(0, cumsum(utils::head(value, -1) * diff(time))) / time) %>%
    dplyr::ungroup()

  if (is.list(envs)) env <- envs[[1]]
  else env <- envs

  plot_obj <-
    ggplot(monitor_data) +
    aes_(x = ~time, color = ~item) +
    geom_line(aes_(y = ~mean, group = ~interaction(replication, item))) +
    geom_step(aes_(y = ~value, group = ~interaction(replication, item)), limits, lty = 2) +
    ggtitle(paste("Resource usage:", resource_name)) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y = 0)

  if (steps == T) {
    plot_obj <- plot_obj +
      geom_step(aes_(y = ~value, group = ~interaction(replication, item)), alpha = .4)
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
  if (is.list(envs)) env <- envs[[1]]
  else env <- envs

  monitor_data <- envs %>% get_mon_resources(data = "counts") %>%
    dplyr::filter_(~resource %in% resources) %>%
    tidyr::gather_("item", "value", c("server", "queue", "system")) %>%
    dplyr::mutate_(item = ~factor(item)) %>%
    dplyr::filter_(~item == "server") %>%
    dplyr::group_by_(~resource) %>%
    dplyr::mutate_(capacity = ~get_capacity(env, resource[[1]])) %>%
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
