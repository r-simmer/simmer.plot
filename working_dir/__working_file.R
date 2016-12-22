library(simmer)

t0 <- trajectory() %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  branch(function() 1, TRUE,
         trajectory() %>%
           seize("doctor", function() 1) %>%
           timeout(function() rnorm(1, 20)) %>%
           release("doctor", function() 1) %>%
           branch(function() 1, TRUE,
                  trajectory() %>%
                    seize("administration", 1) %>%
                    timeout(1) %>%
                    release("administration", 1))) %>%
  rollback(1) %>%
  rollback(1, check = function() FALSE) %>%
  set_attribute("dummy", 1) %>%
  set_attribute("dummy", function() 1)

########################################################

library(geomnet)

t0$verbose <- TRUE
out <- utils::capture.output(t0)
