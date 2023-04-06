test_that("errors are issued", {
  resources <- get_mon_resources(simmer())
  expect_is(resources, "data.frame")
  expect_is(resources, "resources")
  expect_error(plot(resources), "no data available")
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

  resources <- get_mon_resources(reps)

  expect_is(plot(resources, "usage", "doctor"), "ggplot")
  expect_is(plot(resources, "usage", c("nurse", "doctor")), "ggplot")
  expect_is(plot(resources, "usage", "doctor", items = "server"), "ggplot")
  expect_is(plot(resources, "usage", "doctor", items = "server", steps = TRUE), "ggplot")
  expect_is(plot(resources, "utilization", "nurse"), "ggplot")
  expect_is(plot(resources, "utilization", c("nurse", "doctor", "administration")), "ggplot")
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

  resources <- get_mon_resources(reps)

  expect_is(plot(resources, "usage", "doctor"), "ggplot")
  expect_is(plot(resources, "usage", c("nurse", "doctor")), "ggplot")
  expect_is(plot(resources, "usage", "doctor", items = "server"), "ggplot")
  expect_is(plot(resources, "usage", "doctor", items = "server", steps = TRUE), "ggplot")
  expect_is(plot(resources, "utilization", "nurse"), "ggplot")
  expect_is(plot(resources, "utilization", c("nurse", "doctor", "administration")), "ggplot")
})

test_that("resources and items are filtered and plotted in the specified order", {
  resources <- simmer() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
    run(80) %>%
    get_mon_resources()

  fct <- c("nurse", "doctor")
  p <- plot(resources, "utilization", fct)
  expect_equal(levels(p$data$resource), fct)

  fct <- c("system", "queue")
  p <- plot(resources, metric="usage", "doctor", items = fct)
  expect_equal(levels(p$data$item), fct)
})
