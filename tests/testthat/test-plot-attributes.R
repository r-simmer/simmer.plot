context("plot_attributes")

t0 <-
  trajectory() %>%
  set_attribute("my_attr1", function() runif(1)) %>%
  set_attribute("my_attr2", function() runif(1))

test_that("single replication plots", {
  reps <- simmer() %>%
    add_generator("frog", t0, function() rnorm(1, 10, 2), mon = 2) %>%
    run(80)

  expect_is(plot(reps, "attributes"), "ggplot")
  expect_is(plot(reps, "attributes", keys="my_attr1"), "ggplot")
  expect_is(plot(reps, "attributes", keys=c("my_attr1", "my_attr2")), "ggplot")
})

test_that("multiple replication plots", {
  reps <- lapply(1:100, function(i) {
    simmer() %>%
      add_generator("frog", t0, function() rnorm(1, 10, 2), mon = 2) %>%
      run(80)
  })

  expect_is(plot(reps, "attributes"), "ggplot")
  expect_is(plot(reps, "attributes", keys="my_attr1"), "ggplot")
  expect_is(plot(reps, "attributes", keys=c("my_attr1", "my_attr2")), "ggplot")
})
