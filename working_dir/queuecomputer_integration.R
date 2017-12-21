################################################################################
# wrapper
################################################################################

QCWrap <- R6::R6Class("wrap",
  public = list(
    initialize = function(dp) private$dp <- dp,

    get_mon_arrivals = function(...) {
      private$dp$departures_df %>%
        dplyr::rename(start_time = arrivals) %>%
        dplyr::mutate(end_time = start_time + system_time,
                      activity_time = system_time - waiting,
                      finished = TRUE)
    },

    get_mon_resources = function(...) {
      private$dp$queuelength_df %>%
        dplyr::filter(times != dplyr::lead(times, default="1")) %>%
        dplyr::rename(queue = queuelength) %>%
        dplyr::right_join(private$dp$systemlength_df, by="times") %>% # this produces NAs, I don't understand why
        tidyr::fill(queue) %>%                                        # I tried to fix them with fill(), but the result is not ok
        dplyr::rename(time = times, system = queuelength) %>%
        dplyr::mutate(resource = "QC",
                      server = system - queue,
                      capacity = private$dp$servers_input,
                      queue_size = Inf,
                      limit = Inf)
    }
  ),
  private = list( dp = NULL )
)

qcwrap <- function(dp) QCWrap$new(dp)

################################################################################
# example
################################################################################

library(queuecomputer)
library(simmer.plot)

set.seed(42)
arrivals <- cumsum(rexp(100))
service <- rexp(100)
departures <- queue_step(arrivals = arrivals, service = service)

resources <- get_mon_resources(qcwrap(departures))
arrivals <- get_mon_arrivals(qcwrap(departures))

plot(arrivals)                                    # ok
plot(resources, metric="usage", "QC")             # not ok
plot(resources, metric="usage", "QC", steps=TRUE) # not ok

################################################################################

mm1 <- trajectory() %>%
  seize("QC") %>%
  timeout(function() rexp(1)) %>%
  release("QC")

set.seed(42)
arrivals <- cumsum(rexp(100))
env <- simmer() %>%
  add_resource("QC") %>%
  add_generator("arrival", mm1, at(arrivals)) %>%
  run(Inf)

resources <- get_mon_resources(env)
arrivals <- get_mon_arrivals(env)

plot(arrivals)
plot(resources, metric="usage", "QC")
plot(resources, metric="usage", "QC", steps=TRUE)
