# Set up the ts_obj fitting object

Set up the ts_obj fitting object

## Usage

``` r
setup_fitting_ts(
  ts_obj,
  pfpr,
  jdates,
  yr0 = 2015,
  t_neg_inf = -365,
  N = 1,
  gof_method = "ts_sse"
)
```

## Arguments

- ts_obj:

  a **`ramp.xds`** model object

- pfpr:

  a *Pf*PR time series

- jdates:

  julian dates for `pfpr`

- yr0:

  the starting year

- t_neg_inf:

  a starting year

- N:

  the number of interpolation points for hindcasting and forecasting

- gof_method:

  to dispatch
  [compute_gof](https://dd-harp.github.io/ramp.work/reference/compute_gof.md)

## Value

a **`ramp.xds`** model object
