# Round Values while Preserving their Rounded Sum

Round Values while Preserving their Rounded Sum

## Usage

``` r
ps_round_preserve_sum(x, digits = 0L)
```

## Source

<http://biostatmatt.com/archives/2902>

## Arguments

- x:

  A numeric vector of values to round

- digits:

  A count of the number of decimals for rounding

## Examples

``` r
sum(c(0.33, 0.33, 0.33))
#> [1] 0.99
round(c(0.33, 0.33, 0.33), 1)
#> [1] 0.3 0.3 0.3
sum(round(c(0.33, 0.33, 0.33), 1))
#> [1] 0.9
ps_round_preserve_sum(c(0.33, 0.33, 0.33), 1)
#> [1] 0.3 0.3 0.4
sum(ps_round_preserve_sum(c(0.33, 0.33, 0.33), 1))
#> [1] 1
```
