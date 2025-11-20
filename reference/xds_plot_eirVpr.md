# xds_plot EIR(t) *vs.* the PR(t)

xds_plot EIR(t) *vs.* the PR(t)

## Usage

``` r
xds_plot_eirVpr(
  xds_obj,
  i = 1,
  clrs = "black",
  llty = 1,
  stable = FALSE,
  add_axes = TRUE
)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- i:

  the host species index

- clrs:

  color(s) for xds_plotting

- llty:

  an [integer](https://rdrr.io/r/base/integer.html) that specifies `lty`
  for xds_plotting

- stable:

  a [logical](https://rdrr.io/r/base/logical.html) set to FALSE for
  `orbits` and TRUE for `stable_orbits`

- add_axes:

  a [logical](https://rdrr.io/r/base/logical.html) to xds_plot add_axes
  only if FALSE
