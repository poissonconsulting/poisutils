
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/joethorley/stability-badges#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/poissonconsulting/poisutils.svg?branch=master)](https://travis-ci.org/poissonconsulting/poisutils)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/poisutils?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/poisutils)
[![Coverage
Status](https://img.shields.io/codecov/c/github/poissonconsulting/poisutils/master.svg)](https://codecov.io/github/poissonconsulting/poisutils?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/poisutils)](https://cran.r-project.org/package=poisutils)

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

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/poissonconsulting/poisutils/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
