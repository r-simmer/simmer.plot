# simmer.plot 0.1.4

## Minor changes and fixes

* Fix trajectory parsing (da45863). Different compilers represent NULL pointers in different ways: `0`, `(nil)`... 

# simmer.plot 0.1.3

## Minor changes and fixes

* Reduce the footprint caused by the "Plotting simmer trajectories" vignette, thus solving also a WARN in Solaris builds (9cb833a).
* Assign reproducible identifiers to nodes instead of using external pointers in `plot.trajectory()`. The DOT output can be verified regardless of the ordering (3c3bc9b).

# simmer.plot 0.1.1

## New features

* This package detaches plotting functionalities from the `simmer` package (present in versions 3.5.x and below). The old `plot_resource_usage()`, `plot_resource_utilization()`, `plot_evolution_arrival_times()` and `plot_attributes()` are implemented as an S3 method for the `plot()` generic (see `?plot.simmer`).
* An S3 method for plotting diagrams of trajectories is also provided (see `?plot.trajectory`).
