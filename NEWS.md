# simmer.plot 0.1.1

## New features

* This package detaches plotting functionalities from the `simmer` package (present in versions 3.5.x and below). The old `plot_resource_usage()`, `plot_resource_utilization()`, `plot_evolution_arrival_times()` and `plot_attributes()` are implemented as an S3 method for the `plot()` generic (see `?plot.simmer`).
* An S3 method for plotting diagrams of trajectories is also provided (see `?plot.trajectory`).
