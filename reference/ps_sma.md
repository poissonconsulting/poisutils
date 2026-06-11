# Simple Moving Average

Returns the simple moving average with a window of `2 * n`.

## Usage

``` r
ps_sma(x, n = 1L, na.rm = FALSE)
```

## Arguments

- x:

  An integer or double vector.

- n:

  A positive integer of the window before and after each value.

- na.rm:

  A flag indicating whether to ignore missing values when calculating
  the mean.

## Value

A double vector.

## Examples

``` r
ps_sma(1:4)
#> [1] 1.5 2.0 3.0 3.5
```
