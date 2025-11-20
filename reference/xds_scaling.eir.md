# Compute eir-pr scaling relationships

This function calls
[ramp.xds::xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.html)
computes average annual values for the eir, the pr, and other
interesting terms and returns a table. It is computed for a xds_obj of
class "cohort"

## Usage

``` r
# S3 method for class 'eir'
xds_scaling(xds_obj, N = 25)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- N:

  an integer

## Value

an **`xds`** model object
