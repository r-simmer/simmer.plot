context("plot.simmer")

test_that("S3 plot.simmer works as expected", {
  expect_error(plot(simmer(), "asdf"),
               ".*should be one of .*resources.*, .*arrivals.*, .*attributes.*")
  expect_error(plot(simmer(), "resources"), "no data available")
  expect_error(plot(simmer(), "arrivals"), "no data available")
  expect_error(plot(simmer(), "attributes"), "no data available")
})

test_that("S3 plot.wrap works as expected", {
  expect_error(plot(wrap(simmer()), "asdf"),
               ".*should be one of .*resources.*, .*arrivals.*, .*attributes.*")
  expect_error(plot(wrap(simmer()), "resources"), "no data available")
  expect_error(plot(wrap(simmer()), "arrivals"), "no data available")
  expect_error(plot(wrap(simmer()), "attributes"), "no data available")
})
