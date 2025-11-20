# Compute Measures of Impact

Compute Measures of Impact

## Usage

``` r
compute_impact(
  history,
  counterfactual,
  times = c(),
  event_date = c(),
  eval_period = 365
)
```

## Arguments

- history:

  a model of the history: a **`ramp.xds`** xds_obj object

- counterfactual:

  a model of the counterfactual: a **`ramp.xds`** xds_obj

- times:

  a set of times to simulate

- event_date:

  the date of an event

- eval_period:

  the date of an event

## Value

a list with measures of impact
