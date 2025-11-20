# Parasite Dispersal through one Parasite Generation (Humans)

Computes a `n` by `n` matrix describing parasite dispersal from
infecteds (columns) to infectees (rows). \$\$\mathcal{R} = b \beta \cdot
{\cal V} \cdot \mbox{diag}\left(W \right) \cdot \beta^T \cdot
\mbox{diag}\left(DH\right)\$\$

## Usage

``` r
make_calR(b, beta, VC_matrix, W, D, H)
```

## Arguments

- b:

  transmission efficiency from mosquitoes to humans

- beta:

  the biting distribution matrix

- VC_matrix:

  parasite dispersal by mosquitoes matrix (see
  [make_VC_matrix](https://dd-harp.github.io/ramp.work/reference/make_VC_matrix.md))

- W:

  ambient human population at each patch

- D:

  human transmitting capacity

- H:

  human population size of each strata

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
