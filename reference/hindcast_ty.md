# Hindcast a Baseline

This function adds N interpolation points to the spline parameters
*before* an observation period for burn-in. The \\t\\ values are set at
yearly intervals, and the \\y\\ values that serve as a basis

## Usage

``` r
hindcast_ty(xds_obj, ix = c())
```

## Arguments

- xds_obj:

  a **`ramp.xds`** xds_obj object

- ix:

  (optional) indices for
  [impute_value](https://dd-harp.github.io/ramp.work/reference/impute_value.md)

## Value

a **`ramp.xds`** xds_obj object
