#' \pkg{simmer.plot}: Plotting Methods for \pkg{simmer}
#'
#' A set of plotting methods for \pkg{simmer} trajectories and simulations.
#'
#' @author Iñaki Ucar, Bart Smeets
#'
#' @seealso \pkg{simmer}'s homepage \url{http://r-simmer.org} and
#' GitHub repository \url{https://github.com/r-simmer/simmer.plot}.
#'
#' @docType package
#' @name simmer.plot
#'
#' @import simmer ggplot2
#' @importFrom graphics plot
NULL

#' Monitoring Statistics
#'
#' Replacements for \code{\link[simmer:get_mon]{get_mon_arrivals}},
#' \code{\link[simmer:get_mon]{get_mon_attributes}} and
#' \code{\link[simmer:get_mon]{get_mon_resources}}.
#' These versions just add a new class (\code{arrivals}, \code{attributes} or
#' \code{resources} respectively) to the resulting data frame.
#'
#' @param ... see \code{\link[simmer]{get_mon}}.
#'
#' @return Returns a data frame of class \code{arrivals}, \code{attributes} or
#' \code{resources}.
#'
#' @name get_mon
NULL

#' Plot Methods for \code{simmer} Monitoring Statistics
#'
#' Methods for the \code{\link{plot}} generic.
#'
#' @param x a data frame of class \code{arrivals}/\code{attributes}/\code{resources}
#' (see \code{\link{get_mon}}).
#' @param metric specific metric to compute.
#'
#' @return Returns a ggplot2 object.
#' @name plot.mon
#' @examples
#' t0 <- trajectory("my trajectory") %>%
#'   ## add an intake activity
#'   seize("nurse", 1) %>%
#'   timeout(function() rnorm(1, 15)) %>%
#'   release("nurse", 1) %>%
#'   ## add a consultation activity
#'   seize("doctor", 1) %>%
#'   timeout(function() rnorm(1, 20)) %>%
#'   release("doctor", 1) %>%
#'   ## add a planning activity
#'   seize("administration", 1) %>%
#'   timeout(function() rnorm(1, 5)) %>%
#'   release("administration", 1)
#'
#' env <- simmer("SuperDuperSim") %>%
#'   add_resource("nurse", 1) %>%
#'   add_resource("doctor", 2) %>%
#'   add_resource("administration", 1) %>%
#'   add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
#'   run(until=80)
#'
#' resources <- get_mon_resources(env)
#' arrivals <- get_mon_arrivals(env)
#'
#' plot(resources, metric="usage", "doctor", items = "server", steps = TRUE)
#' plot(resources, metric="utilization", c("nurse", "doctor", "administration"))
#' plot(arrivals, metric="waiting_time")
#'
NULL
