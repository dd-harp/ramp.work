# Fit IRS Shock

As part of a dynamical time series analysis, fit the parameters
describing the response timeline to IRS. This fits three parameters:

- `size` controls the maximum effect size of the shock

- `d_50` days after the shock when the effect size reaches half of the
  maximum

- `d_shape` a shape parameter

## Usage

``` r
fit_irs_shock(xds_obj, options = list())
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- options:

  setup options for the irs rounds indexing

## Value

a **`ramp.xds`** model object
