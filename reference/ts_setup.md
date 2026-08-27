# Build a Decomposable Time Series Model

Like
[ramp.xds::xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.html),
ts_setup sets up a decomposed, multiplicative time-series analysis that
can be used to analyze malaria *Pf*PR time series data in the same way
as an **`xds`** object.

The **`ts`** object defines `frame = class(frame) = 'ts'`

The interface includes options to configure a function describing `F_pr`
as a function of time, with seasonal components and a trend.

This can be used to model a cohort as it ages; a function is set up to
modify exposure by age.

## Usage

``` r
ts_setup(
  pr = 0.3,
  season_par = makepar_F_c(1),
  trend_par = makepar_F_c(1),
  age_par = makepar_F_c(1),
  shock_par = makepar_F_c(1),
  tnorm = 365
)
```

## Arguments

- pr:

  is the mean pr

- season_par:

  parameters to configure a seasonality function using
  [ramp.func::make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html)

- trend_par:

  parameters to configure a trends function using
  [ramp.func::make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html)

- age_par:

  parameters to configure an age weights function using
  [ramp.func::make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html)

- shock_par:

  parameters to configure a shock using
  [ramp.func::make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html)

- tnorm:

  normalize from 0 up to tnorm

## Value

a **`ts`** object
