# simmer.plot 0.1.8

## Minor changes and fixes

* Fix forks with a single path (5fa5937).
* Re-arrange dataframes in tests (6cc4f0a).
* Fix trajectory parsing with another NULL pointer format: `0x0` (#4).

# simmer.plot 0.1.7

## Minor changes and fixes

* Fix trajectory parsing (8295c52). Parsing DOT is definitely unreliable. Rely entirely on DiagrammeR.

# simmer.plot 0.1.6

## Minor changes and fixes

* More robust testing (d4879ee, trying to address #2).
* Visual improvements for `plot.trajectory()` (9b49f83, d84a469).
* Assign the dashed-gray edge style to subtrajectories in a consistent way (addressing #3).
* Fix some defaults (8a7fb7d).
* Do not plot cumulative average and steps at the same time for resources usage (89040bd).
* Change alpha with the number of replications (60beae2).

# simmer.plot 0.1.5

## Minor changes and fixes

* Fix plot of trajectories without forks or rollbacks (332ec61).
* Fix plot of trajectories with a single node (f787c95).
* Fail gracefully with empty trajectories (2d42d7f).
* Fail gracefully if `simmer` doesn't return pointers (3be41cf, a60a641). This should not happen, though.
* Improve tests (a9e7fa0).
* Update to DiagrammeR 0.9.0 (dbc01b8).

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
