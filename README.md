
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/joethorley/stability-badges#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/poisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/poisutils/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/poisutils/graph/badge.svg)](https://app.codecov.io/gh/poissonconsulting/poisutils)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/license/mit/)
<!-- badges: end -->

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
