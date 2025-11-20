# Time Since Event

Compute the time elapsed since the last vector control event for all the
spline \\t\\-values

## Usage

``` r
time_since_event(xds_obj, bednet_ix = list(), irs_ix = list())
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- bednet_ix:

  list of events to fit; if empty, then fit all

- irs_ix:

  list of events to fit; if empty, then fit all

## Value

numeric
