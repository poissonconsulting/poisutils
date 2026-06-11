# poisutils

An R package of utility functions for Poisson Consulting’s scripts and
packages.

## Demonstration

``` r

library(poisutils)

x <- 1
is.named(x)
#> [1] FALSE
names(x) <- "one"
is.named(x)
#> [1] TRUE
```

## Installation

``` r

# install.packages("devtools")
devtools::install_github("poissonconsulting/poisutils")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/poisutils/issues).

[Pull requests](https://github.com/poissonconsulting/poisutils/pulls)
are always welcome.

## Code of Conduct

Please note that the poisutils project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
