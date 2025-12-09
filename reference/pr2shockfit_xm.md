# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
pr2shockfit_xm(
  xds_obj,
  rep = 1,
  trend_fix = c(),
  trend_x = 1,
  bednet_fix = c(),
  bednet_x = 0,
  irs_fix = c(),
  irs_x = 0
)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- rep:

  number of times to repeat the fitting cycle

- trend_fix:

  the indices for trend events to fix

- trend_x:

  the fixed values

- bednet_fix:

  the indices for bednet events to fix

- bednet_x:

  the fixed values

- irs_fix:

  the indices for irs events to fix

- irs_x:

  the fixed values

## Value

the **`ramp.xds`** model object, fitted to a time series. The state at
the end is saved as `xds_obj$history`
