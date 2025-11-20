# feature Interpolation Points

This function copies the interpolation points from the `hindcast` and
`data_obj` and `forecast` to `fit_obj.`

During fit_obj, the algorithm changes one or more of the \\y\\ values of
the interpolation points in `xds_obj$data_obj$yy.` The xds_obj then
calls `hindcast_ty` and `forecast_ty` to feature `xds_obj$hindcast$y`
(the pre-observation interpolation points for burn-in) and
`xds_obj$hindcast$y` (post-observation interpolation points). These are
copied to the full set of control points stored at at
`xds_obj$fit_obj$tt` and `xds_obj$fit_obj$yy.`

The rules for `hindcast_ty` and `forecast_ty` are setup by
`setup_hindcast` and `setup_forecast.`

## Usage

``` r
update_fit_trend(xds_obj)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** xds_obj object

## Value

a **`ramp.xds`** xds_obj object
