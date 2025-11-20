# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
pr2history_xm(xds_obj, twice = FALSE)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- twice:

  cycle through a second time

## Value

the **`ramp.xds`** model object, fitted to a time series. The state at
the end is saved as `xds_obj$history`
