# Proportional Change to n-Fold Change

Proportional Change to n-Fold Change

## Usage

``` r
ps_prop2nfold_change(x)
```

## Arguments

- x:

  A numeric vector of the proportional change

## Value

A numeric vector of the n-fold change

## Examples

``` r
ps_nfold_change(3, c(3, 1, 9))
#> [1]  0 -2  2
ps_prop_change(3, c(3, 1, 9))
#> [1]  0.0000000 -0.6666667  2.0000000
ps_prop2nfold_change(ps_prop_change(3, c(3, 1, 9)))
#> [1]  0 -2  2
```
