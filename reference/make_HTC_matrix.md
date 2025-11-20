# Parasite dispersal by humans

Compute the `p` by `p` matrix \\\mathcal{D}\\ whose columns describe how
potentially infectious person time from persons in that patch are
dispersed across other patches. \$\$\mathcal{D} = \mbox{diag}\left(W
\right) \cdot \beta^T \cdot \mbox{diag}\left(bDH\right) \cdot \beta\$\$

## Usage

``` r
make_HTC_matrix(W, beta, b, D, H)
```

## Arguments

- W:

  ambient human population at each patch

- beta:

  the biting distribution matrix

- b:

  transmission efficiency from mosquitoes to humans

- D:

  human transmitting capacity

- H:

  human population size of each strata

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
