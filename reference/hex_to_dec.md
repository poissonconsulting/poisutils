# Hexadecimal to Decimal Code Converter

Hexadecimal to Decimal Code Converter

## Usage

``` r
hex_to_dec(hex)
```

## Arguments

- hex:

  A string of the hex code.

## Value

The dec code as a character.

## Details

The function will return NA if non HEX letters are present. The only
letters that can be present in the HEX codes are A to F.

## Examples

``` r
hex_to_dec("349EA72A50")
#> [1] "226000054864"
hex_to_dec("14A5D0BE89")
#> [1] "88681266825"
hex_to_dec(c("14D", "E67"))
#> [1] "333"  "3687"
hex_to_dec(c("14D", "E67", NA))
#> [1] "333"  "3687" NA    
```
