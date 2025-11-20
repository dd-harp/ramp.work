# Get interpolation points

The `ix` indicates the interpolation points that were not modified by
control. The optional argument `opts$t` should be the time of an event,
so the interpolation points are before the event and unmodified by
control.

## Usage

``` r
# S3 method for class 'tix'
get_trend_ty(ix, xds_obj, trust_ty)
```

## Arguments

- ix:

  index or indices of interpolation points

- xds_obj:

  a **`ramp.xds`** xds_obj object

- trust_ty:

  a text string to dispatch `get_trend_ty`

## Value

Interpolation points, \\t,y\\, as a list
