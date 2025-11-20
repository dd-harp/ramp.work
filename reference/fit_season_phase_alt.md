# Fit the phase

For a *Pf*PR time series, \\x\\, fit the phase for a seasonal pattern.
The algorithm works by choosing an initial mesh, around 46 days, and
then shrinking the mesh around the value that maximizes the goodness of
fit. The \\n\\ parameter determines how many times the process is
iterated. For \\n=2\\, the mesh is

## Usage

``` r
fit_season_phase_alt(xds_obj, n = 2)
```

## Arguments

- xds_obj:

  an `xds` xds_obj

- n:

  number of times to iterate

## Value

a list with the mean peak and the values
