context("plot.arrivals")

test_that("errors are issued", {
  arrivals <- get_mon_arrivals(simmer())
  expect_is(arrivals, "data.frame")
  expect_is(arrivals, "arrivals")
  expect_error(plot(get_mon_arrivals(simmer())), "no data available")
})

t0 <- trajectory("my trajectory") %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

test_that("single replication plots", {
  reps <- simmer() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
    run(80)

  arrivals <- get_mon_arrivals(reps)

  expect_is(plot(arrivals, "flow_time"), "ggplot")
  expect_is(plot(arrivals, "activity_time"), "ggplot")
  expect_is(plot(arrivals, "waiting_time"), "ggplot")
})

test_that("multiple replication plots", {
  reps <- lapply(1:100, function(i) {
    simmer() %>%
      add_resource("nurse", 1) %>%
      add_resource("doctor", 2) %>%
      add_resource("administration", 1) %>%
      add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
      run(80)
  })

  arrivals <- get_mon_arrivals(reps)

  expect_is(plot(arrivals, "flow_time"), "ggplot")
  expect_is(plot(arrivals, "activity_time"), "ggplot")
  expect_is(plot(arrivals, "waiting_time"), "ggplot")
})
