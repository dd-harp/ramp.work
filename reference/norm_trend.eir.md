# Normalize Trend

Adjust the y-values for the interpolation point and reset the mean daily
EIR such that the product of the seasonal pattern and the trend over an
observation period is one.

## Usage

``` r
# S3 method for class 'eir'
norm_trend(xds_obj)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** object

## Value

a number
