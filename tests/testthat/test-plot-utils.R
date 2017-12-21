context("utils")

test_that("the method dispatcher works as expected", {
  foo <<- function(x, y, z) UseMethod("foo")
  foo.bar <<- function(x, y, z) dispatch_next(y, x, z)
  foo.bar.baz <<- function(x, z) c(x, z)
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
