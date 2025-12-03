# Initialize the phase parameter

Compute the empirical phase from a *Pf*PR time series, \\t,x\\, and
adjust the model phase to match it.

## Usage

``` r
preset_phase(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** object

## Value

an **`xds`** object

## Note

This algorithm makes a crude guess at the seasonal phase for the forcing
function. *close* to the value that will get fitted later.
