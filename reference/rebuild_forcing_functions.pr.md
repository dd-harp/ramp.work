# Rebuild Forcing Functions

Rebuild forcing functions on the EIR object using `make_function`:

- `F_season` is made from `season_par`

- `F_trend` is made from `trend_par`

- `F_shock` is made from `shock_par`

## Usage

``` r
# S3 method for class 'pr'
rebuild_forcing_functions(xds_obj, ix = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- ix:

  the species index

## Value

an **`xds`** object
