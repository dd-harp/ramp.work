# Scaling for Malaria Metrics

Compute scaling relationships for malaria metrics

## Usage

``` r
xds_scaling(xds_obj, N = 30)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- N:

  an integer

## Value

an **`xds`** model object

## Note

This function dispatches on `xds_obj$forced_by`
