# Replace Values in a List

Replace Values in a List

## Usage

``` r
# S3 method for class 'numeric'
modify_vector_X(V, ixV, X, ixX)
```

## Arguments

- V:

  the V to modify

- ixV:

  indices to be replaced

- X:

  the new values

- ixX:

  indices to be replaced

## Value

the modified V

## Examples

``` r
modify_vector_X(6, c(1,7), 1:10)
#> Warning: number of items to replace is not a multiple of replacement length
#> [1]  1 NA NA NA NA NA  2
```
