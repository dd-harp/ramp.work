# Reconstruct a history of exposure from a PR time series

Construct a function describing the EIR, including the mean EIR, the
seasonal pattern, and the i

For set of paired a time series \\X,\\ compute the phase of a seasonal
pattern for the EIR

## Usage

``` r
restore_pr2shockit(xds_obj)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

## Value

an `xds` object

## Note

This utility relies on `xds_scaling`
