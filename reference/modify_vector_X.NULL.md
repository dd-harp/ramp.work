# Replace Values in a List

If `ix` is NULL, return `X`

## Usage

``` r
# S3 method for class '`NULL`'
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
modify_vector_X(6:15, c(), 1:10)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
