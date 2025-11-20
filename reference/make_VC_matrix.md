# Parasite dispersal by mosquitoes

Compute the `p` by `p` matrix \\\mathcal{V}\\ whose columns describe how
infective bites arising from all the mosquitoes biting a single human on
a single day are dispersed to other patches, accounting for movement and
mortality. \$\$\mathcal{V} = fq\Omega^{-1} \cdot e^{-\Omega\tau} \cdot
\mbox{diag}\left(\frac{fqM}{W}\right)\$\$

## Usage

``` r
make_VC_matrix(f, q, Omega, tau, M, W)
```

## Arguments

- f:

  the feeding rate

- q:

  fraction of bloodmeals taken on humans

- Omega:

  the mosquito demography matrix

- tau:

  duration of the extrinsic incubation period

- M:

  size of mosquito population in each patch

- W:

  ambient human population at each patch

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
