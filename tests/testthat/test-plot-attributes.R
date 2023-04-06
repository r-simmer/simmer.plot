test_that("errors are issued", {
  attributes <- get_mon_attributes(simmer())
  expect_is(attributes, "data.frame")
  expect_is(attributes, "attributes")
  expect_error(plot(get_mon_attributes(simmer())), "no data available")
})

t0 <-
  trajectory() %>%
  set_attribute("my_attr1", function() runif(1)) %>%
  set_attribute("my_attr2", function() runif(1))

test_that("single replication plots", {
  reps <- simmer() %>%
    add_generator("frog", t0, function() rnorm(1, 10, 2), mon = 2) %>%
    run(80)

  attributes <- get_mon_attributes(reps)

  expect_is(plot(attributes), "ggplot")
  expect_is(plot(attributes, keys="my_attr1"), "ggplot")
  expect_is(plot(attributes, keys=c("my_attr1", "my_attr2")), "ggplot")
})

test_that("multiple replication plots", {
  reps <- lapply(1:100, function(i) {
    simmer() %>%
      add_generator("frog", t0, function() rnorm(1, 10, 2), mon = 2) %>%
      run(80)
  })

  attributes <- get_mon_attributes(reps)

  expect_is(plot(attributes), "ggplot")
  expect_is(plot(attributes, keys="my_attr1"), "ggplot")
  expect_is(plot(attributes, keys=c("my_attr1", "my_attr2")), "ggplot")
})
