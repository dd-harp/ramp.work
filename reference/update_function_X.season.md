# feature `F_season`

This features three shape parameters for a function `F_season` using
[modify_vector_X](https://dd-harp.github.io/ramp.work/reference/modify_vector_X.md)
and
[ramp.xds::change_season](https://dd-harp.github.io/ramp.xds/reference/change_season.html)

## Usage

``` r
# S3 method for class 'season'
update_function_X(X, xds_obj, feature = "season", options = list())
```

## Arguments

- X:

  new parameter values

- xds_obj:

  a **`ramp.xds`** xds_obj object

- feature:

  a string to dispatch

- options:

  a list to configure feature-specific indexing

## Value

sum of squared differences

## Note

This assumes the vector, `X`, has got three sets of parameters
describing three parameters in a seasonal function with parameter names
phase, bottom, and pw.
