#' @name get_mon
#' @export
get_mon_attributes <- function(...) {
  x <- simmer::get_mon_attributes(...)
  class(x) <- c("attributes", class(x))
  x
}

#' @name plot.mon
#' @param keys attributes to plot (if left empty, all attributes are shown).
#' @export
plot.attributes <- function(x, metric=NULL, keys, ...) {
  if (!missing(keys))
    x <- dplyr::filter(x, .data$key %in% keys)
  if (nrow(x) == 0)
    stop("no data available or 'keys' not found")

  plot_obj <-
    ggplot(x) +
    aes_(x = ~time, y = ~value) +
    geom_step(aes_(group = ~replication), alpha = set_alpha(x)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("value") +
    expand_limits(y = 0)

  if (length(unique(x$key)) > 1) {
    plot_obj <- plot_obj +
      ggtitle("Attribute evolution") +
      facet_wrap(~key, scales = "free_y")
  } else {
    plot_obj <- plot_obj +
      ggtitle(paste0("Attribute evolution: ", x$key[[1]]))
  }

  plot_obj
}
