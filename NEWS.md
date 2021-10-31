# simmer.plot 0.1.17

- Fix calculation of resource utilization when zero capacity values are present
  (#22 addressing #21; #24).
- Expose `limits` in `plot.resources(metric="usage")`, improve documentation of
  method-specific options (#25).

# simmer.plot 0.1.16

- Fix rollback pointer with an infinite amount (#18).
- Workaround bug in DiagrammeR (#19).
- Workaround bug in ggplot2 (#20).

# simmer.plot 0.1.15

- Fix test for simmer v4.2.2.

# simmer.plot 0.1.14

- Arrange factors (`names` and `items` in `plot.resources`) in the order
  provided (#15).
- Improve documentation (#16).

# simmer.plot 0.1.13

- Update to DiagrammeR 1.0.0 (7c2b9c7).

# simmer.plot 0.1.12

- New S3 methods for plotting monitoring statistics, which enable us to call
  `plot()` on the output of `get_mon_arrivals()`, `get_mon_attributes()` and
  `get_mon_resources()` (see `?plot.mon`). Calling `plot()` on the `simmer`
  environment will continue to work, but it is deprecated (#11).
- Better defaults for resources and attributes: all names/keys are plotted if
  nothing is specified (#13).

# simmer.plot 0.1.11

- Fix trajectory parsing with variable-length pointers (#9).
- Update to tidyr 0.7.x (da58045).
- Update to DiagrammeR 0.9.1 (247aa4f).

# simmer.plot 0.1.10

- Fix resource utilization plot with varying resource capacity (#8).
- Fix trajectory parsing with yet another NULL pointer format: `(nil)` (#4).
- Update to dplyr 0.7.x (537cdea).

# simmer.plot 0.1.9

- Workaround for Solaris SPARC (#2).
- Remove ignored edges in `clone()` (#6).
- Add optional verbose info in `plot.trajectory()` labels (#5).

# simmer.plot 0.1.8

- Fix forks with a single path (5fa5937).
- Re-arrange dataframes in tests (6cc4f0a).
- Fix trajectory parsing with another NULL pointer format: `0x0` (#4).

# simmer.plot 0.1.7

- Fix trajectory parsing (8295c52). Parsing DOT is definitely unreliable. Rely
  entirely on DiagrammeR.

# simmer.plot 0.1.6

- More robust testing (d4879ee, trying to address #2).
- Visual improvements for `plot.trajectory()` (9b49f83, d84a469).
- Assign the dashed-gray edge style to subtrajectories in a consistent way
  (addressing #3).
- Fix some defaults (8a7fb7d).
- Do not plot cumulative average and steps at the same time for resources usage
  (89040bd).
- Change alpha with the number of replications (60beae2).

# simmer.plot 0.1.5

- Fix plot of trajectories without forks or rollbacks (332ec61).
- Fix plot of trajectories with a single node (f787c95).
- Fail gracefully with empty trajectories (2d42d7f).
- Fail gracefully if `simmer` doesn't return pointers (3be41cf, a60a641). This
  should not happen, though.
- Improve tests (a9e7fa0).
- Update to DiagrammeR 0.9.0 (dbc01b8).

# simmer.plot 0.1.4

- Fix trajectory parsing (da45863). Different compilers represent NULL pointers
  in different ways: `0`, `(nil)`... 

# simmer.plot 0.1.3

- Reduce the footprint caused by the "Plotting simmer trajectories" vignette, 
  thus solving also a WARN in Solaris builds (9cb833a).
- Assign reproducible identifiers to nodes instead of using external pointers in
  `plot.trajectory()`. The DOT output can be verified regardless of the ordering
  (3c3bc9b).

# simmer.plot 0.1.1

- This package detaches plotting functionalities from the `simmer` package
  (present in versions 3.5.x and below). The old `plot_resource_usage()`,
  `plot_resource_utilization()`, `plot_evolution_arrival_times()` and
  `plot_attributes()` are implemented as an S3 method for the `plot()` generic
  (see `?plot.simmer`).
- An S3 method for plotting diagrams of trajectories is also provided (see
  `?plot.trajectory`).
