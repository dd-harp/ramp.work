# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
pr2shockit_xm(xds_obj, bednet_ix = c(), irs_ix = c(), N = 1)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- bednet_ix:

  the indices for bednet events to fit

- irs_ix:

  the indices for irs events to fit

- N:

  cycle through a N times

## Value

the **`ramp.xds`** model object, fitted to a time series. The state at
the end is saved as `xds_obj$history`
