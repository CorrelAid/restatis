
<!-- README.md is generated from README.Rmd. Please edit that file -->

# restatis

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/restatis)](https://CRAN.R-project.org/package=restatis)
[![R-CMD-check](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/CorrelAid/restatis/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/CorrelAid/restatis/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of restatis is to …

## Installation

You can install the development version of restatis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CorrelAid/restatis")
```

## Authentication

To access the Destatis GENESIS API, you need to have an account at
<https://www-genesis.destatis.de> and store your username and password
for use in R with `gen_auth_save()` (see `?gen_auth_save` for more
details).

## Disclaimer

This package is not in any way affiliated to Destatis (i.e., the German
National Office of Statistics). It is a simple wrapper providing R
functions to the Office’s API. The package creators are not in any way
responsible for the data that can be fetched using its functions. The
license of this package solely applies to its source code.
