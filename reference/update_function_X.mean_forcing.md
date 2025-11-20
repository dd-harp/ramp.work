# Update Mean Forcing

Update the mean value that forces a xds_obj. This dispatches on
class(xds_obj\$forced_by)

## Usage

``` r
# S3 method for class 'mean_forcing'
update_function_X(X, xds_obj, feature, options)
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
