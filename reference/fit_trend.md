# Fit interannual variability using splines

For a *Pf*PR time series, \\x\\, fit a spline function. To fit only a
subset of the interpolation points, define `options$trend_ix`

## Usage

``` r
fit_trend(xds_obj, options = list())
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- options:

  a list to configure the indices

## Value

a list with the mean peak and the values
