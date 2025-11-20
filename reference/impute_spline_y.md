# Impute the baseline

Impute the a new \\y\\ value(s) for one or more interpolation points

## Usage

``` r
impute_spline_y(impute_ix, trusted_ix, xds_obj, impute_y = "mean")
```

## Arguments

- impute_ix:

  the index (or indices) of the \\y\\ values to replace

- trusted_ix:

  the index (or indices) of a node (or nodes) to replace

- xds_obj:

  a **`ramp.xds`** model object

- impute_y:

  a text string to dispatch `impute_value`

## Value

an **`ramp.xds`** model object
