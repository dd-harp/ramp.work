# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
reconD_shock_xm(
  xds_obj,
  do_bednet = TRUE,
  bednet_ix = c(),
  do_irs = TRUE,
  irs_ix = c(),
  D = 365,
  N = 2
)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- do_bednet:

  if TRUE then fit bednet shocks

- bednet_ix:

  indices of events to fit

- do_irs:

  if TRUE then fit irs shocks

- irs_ix:

  indices of events to fit

- D:

  remove spline points less than D days after an event

- N:

  cycle through fits N times

## Value

the **`ramp.xds`** model object, fitted to a time series. The state at
the end is saved as `xds_obj$history`
