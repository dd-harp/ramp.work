# Fit mean forcing

Fit the mean forcing parameter. Functions called by `fit_mean_forcing`
dispatch on `class(xds_obj$forced_by)`

- `eir` updates the mean daily EIR

- `Lambda` updates the mean daily emergence rate

The method is designed for fitting a model to observed values of *Pf*PR
in a time series, \\x_t.\\

## Usage

``` r
fit_mean_forcing(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Note

The function parameter `options` is not used.
