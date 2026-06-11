# Convert Hexidecimal to Decimal PIT Tags

Convert Hexidecimal to Decimal PIT Tags

## Usage

``` r
ps_h2d_tag(hex, country_code = 900)
```

## Arguments

- hex:

  A string of the hex code.

- country_code:

  Number of the country code to add to the decimal code.

## Value

A string.

## Details

Any decimal codes 13 or more digits long will return NA. If the code is
12 digits long the country code is appended directly to the decimal
value. If the decimal code is 11 digits or less then the decimal code is
padded with zeros on the front to ensure the code is 12 digits long
before appending the country code to the code.

## Examples

``` r
ps_h2d_tag("349EA72A50")
#> [1] "900226000054864"
ps_h2d_tag("349EA70")
#> [1] "900000055175792"
ps_h2d_tag("349EA72A50000")
#> [1] NA
ps_h2d_tag("349EA72A50", country_code = 124)
#> [1] "124226000054864"
```
