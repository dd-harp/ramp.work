# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
pr2shockit(xds_obj, fit_method = NULL)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- fit_method:

  the method for [optim](https://rdrr.io/r/stats/optim.html)

## Value

the **`ramp.xds`** model object, fitted to a time series. The state at
the end is saved as `xds_obj$history`
