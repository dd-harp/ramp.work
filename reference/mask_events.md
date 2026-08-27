# Mask data around events

Mask the data around events, parameters control the number of days
before (negative values are OK) and after.

## Usage

``` r
mask_events(xds_obj, daysb4 = 0, days_after = 365)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- daysb4:

  the number of days before the event to mask

- days_after:

  the number of days after the event to mask

## Value

an **`xds_obj`**
