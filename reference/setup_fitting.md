# Set up the xds_obj fitting object

Set up the xds_obj fitting object

## Usage

``` r
setup_fitting(
  xds_obj,
  pfpr,
  jdates,
  yr0 = 2015,
  t_neg_inf = 3650,
  N = 10,
  pr_diagnostic = "rdt",
  gof_method = "sse"
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- pfpr:

  a *Pf*PR time series

- jdates:

  julian dates for `pfpr`

- yr0:

  the starting year

- t_neg_inf:

  a date (in the past) to use for burnin

- N:

  the number of interpolation points for hindcasting and forecasting

- pr_diagnostic:

  the method used to estimate the *Pf*PR data_obj

- gof_method:

  to dispatch
  [compute_gof](https://dd-harp.github.io/ramp.work/reference/compute_gof.md)

## Value

a **`ramp.xds`** model object
