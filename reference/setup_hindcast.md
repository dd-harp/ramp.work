# Hindcast a Baseline

This function adds N interpolation points to the spline parameters
*before* an observation period for burn-in. The \\t\\ values are set at
yearly intervals, and the \\y\\ values that serve as a basis. See
[Hindcasting](https://dd-harp.github.io/ramp.work/vignettes/Hindcast.html)

## Usage

``` r
setup_hindcast(xds_obj, N = 0, method = "use_first")
```

## Arguments

- xds_obj:

  a **`ramp.xds`** xds_obj object

- N:

  the number of years to hindcast

- method:

  to dispatch setup_hindcast_ty

## Value

a **`ramp.xds`** xds_obj object
