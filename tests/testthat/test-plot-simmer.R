context("plot.simmer")

test_that("the method dispatcher works as expected", {
  foo <<- function(x, y, z) UseMethod("foo")
  foo.bar <<- function(x, y, z) dispatch_next(y, x, z)
  foo_baz <<- function(x, z) c(x, z)
  x <- structure(1, class="bar")
  expect_true(all(foo(x, "baz", 2) == c(1, 2)))
})

test_that("S3 plot.list works as expected", {
  plot.foo <<- function(x) x[[1]]
  plot.bar <<- function(x) x[[2]]
  x <- structure(1, class="foo")
  y <- structure(2, class="bar")
  expect_error(plot(list(x, y)))
  expect_true(plot(list(x, x)) == 1)
  expect_true(plot(list(y, y)) == 2)
})

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
