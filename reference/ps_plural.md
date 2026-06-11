# Plural

Plural

## Usage

``` r
ps_plural(x, n = 1L, end = "")
```

## Arguments

- x:

  A string to pluralize.

- n:

  A count of the number of occurrences of x.

- end:

  A string of the last part of the pluralization (useful for adding
  punctuation).

## Value

A string

## Examples

``` r
ps_plural("column", 1)
#> [1] "column"
ps_plural("column", 2)
#> [1] "columns"
ps_plural("column", 3, end = ".")
#> [1] "columns."
```
