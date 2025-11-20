# Make a Forecast from Trusted Values

Use trusted values from a fitted model to generate a new set of
interpolation points

## Usage

``` r
change_forecast_ix(xds_obj, trusted_ix, N = 3, impute_y = "subsamp")
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- trusted_ix:

  the index (or indices) of a node (or nodes) to replace

- N:

  the number of years to forecast

- impute_y:

  a text string to dispatch `impute_value`

## Value

an **`ramp.xds`** model object
