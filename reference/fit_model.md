# Fit a model to data

A function to fit multiple feature of a model at the same time.

The feature of a model to be fit are passed as text strings in a list,
called `feature.`

For some of these feature, `fit_model` can be configured to fit only
some of the parameters or to lump some parameters together (*e.g.* IRS
rounds that use the same pesticide). Each feature sets its own default
for indexing the subset of parameters to be fit, but these can be
overridden by passing text strings in `options.`

If `length(feature) == 1` then there is another `fit_` function in
`ramp.work` that is equivalent to `fit_model` where parameter
constraints and indexing options are described in greater detail. These
are linked in the bullet points below.

If `length(feature) > 1` then `class(feature)= "multifit"` and each
feature in the list gets handled separately.

The function acts only on the following text strings in `feature`:

- **`mean_forcing`** : fits the mean forcing parameter (also, see
  [fit_mean_forcing](https://dd-harp.github.io/ramp.work/reference/fit_mean_forcing.md))

- **`trend`** : fits interpolation points for a spline function (also,
  see
  [fit_trend](https://dd-harp.github.io/ramp.work/reference/fit_trend.md))

  - by default, all \\y\\ values of interpolation points are fitted

  - a subset can be configured by setting `trend_ix` in `options`

  - if `trend` and `mean_forcing` are both called, at least one of the
    interpolation points should be set to `1`

- At most one of the following aspects of seasonality:

  - **`phase`** : the time of year when forcing peaks (also, see
    [fit_season_phase](https://dd-harp.github.io/ramp.work/reference/fit_season_phase.md))

  - **`pw`** : a shape parameter affecting the amplitude (also, see
    [fit_season_pw](https://dd-harp.github.io/ramp.work/reference/fit_season_pw.md))

  - **`bottom`** : a shape parameter affecting amplitude (also, see
    [fit_season_bottom](https://dd-harp.github.io/ramp.work/reference/fit_season_bottom.md))

  - **`amplitude`** : fits the shape parameters that affect amplitude:
    `pw` & `bottom` (also, see
    [fit_season_amplitude](https://dd-harp.github.io/ramp.work/reference/fit_season_amplitude.md))

  - **`season`** : fits all seasonality shape parameters: `pw` &
    `bottom` & `phase` (also, see
    [fit_season](https://dd-harp.github.io/ramp.work/reference/fit_season.md))

- **`irs_coverage`** : fit irs coverage parameters (also see
  [fit_irs_contact](https://dd-harp.github.io/ramp.work/reference/fit_irs_contact.md))

  - by default, coverage levels for all rounds are fitted independently

  - other options can be fitted by setting `irs_ix` in `options`

- **`bednet_contact`** : fit bed net coverage parameters (also see
  [fit_bednet_contact](https://dd-harp.github.io/ramp.work/reference/fit_bednet_contact.md))

  - by default, coverage levels for all rounds are fitted independently

  - other options can be fitted by setting `bednet_ix` in `options`

The **`xds`** model object that gets returned by `fit_model` has the
optimal paramters.

## Usage

``` r
fit_model(xds_obj, feature, options = list(), fit_method = NULL)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- feature:

  a list of one or more model feature to fit

- options:

  a list of options to override feature-specific defaults

- fit_method:

  the method for [optim](https://rdrr.io/r/stats/optim.html)

## Value

an **`xds`** model object
