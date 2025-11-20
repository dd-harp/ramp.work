# Convert the EIR into a vector describing infective biting density

Computes a vector of length `nStrata` describing the daily eir, per
patch from a vector of length `nPatches` describing the daily infective
biting density. \$\$fqZ = \left(\beta^T \cdot \beta \right)^{-1} \cdot
\beta^T \cdot E\$\$

## Usage

``` r
eir2fqZ(eir, beta)
```

## Arguments

- eir:

  a vector describing the EIR in several strata

- beta:

  the mixing matrix

## Value

a numeric [vector](https://rdrr.io/r/base/vector.html)
