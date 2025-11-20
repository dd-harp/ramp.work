# Compute scaling relationships from mosquito emergence through PfPR

This function calls
[ramp.xds::xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.html)
to get the scaling relationships for mosquito density over 9 factors of
10, and average annual values for the eir, the pr, and other interesting
terms and returns a table.

## Usage

``` r
# S3 method for class 'Lambda'
xds_scaling(xds_obj, N = 30)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- N:

  an integer

## Value

**`xds`** xds_obj object
