library(testthat)
library(simmer)
library(simmer.plot)

if (-1 == utils::compareVersion("3.5.1", as.character(utils::packageVersion("simmer"))))
  trajectory <- create_trajectory

test_check("simmer.plot")

detach("package:simmer.plot", unload = TRUE)
detach("package:simmer", unload = TRUE)
