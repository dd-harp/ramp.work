# Setup Imputation

This sets up the the imputatation model object. It sets options for
using interpolation points to impute a baseline or to hindcast or
forecast.

## Usage

``` r
setup_imputation(impute_ty = "mean", trusted_ty = "unmodified")
```

## Arguments

- impute_ty:

  a text string to dispatch `impute_baseline_ty`

- trusted_ty:

  a text string to dispatch `get_trusted_ty`

## Value

an imputation model object
