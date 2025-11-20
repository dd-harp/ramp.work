# Convert a vector describing infective biting density into the EIR, \\E\\

Computes a vector of length `nPatches` describing infective biting
density, per patch, from a vector of length `n` describing the daily EIR
per stratum: \$\$E = \beta \cdot fqZ\$\$

## Usage

``` r
fqZ2eir(fqZ, beta)
```

## Arguments

- fqZ:

  a vector describing infective biting density

- beta:

  the mixing matrix

## Value

a numeric [vector](https://rdrr.io/r/base/vector.html)
