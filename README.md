
<!-- README.md is generated from README.Rmd. Please edit that file -->

# restatis

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/restatis)](https://CRAN.R-project.org/package=restatis)
[![R-CMD-check](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/CorrelAid/restatis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CorrelAid/restatis?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

restatis is a wrapper around the RESTful API that provides access to the
[GENESIS databases of the Federal Statistical Office of Germany
(Destatis)](https://www-genesis.destatis.de/).

## Installation

You can install the released version of restatis from CRAN:

``` r
install.packages("restatis")
```

Or install the development version of restatis from
[GitHub](https://github.com/CorrelAid/restatis) with:

``` r
# install.packages("devtools")
devtools::install_github("CorrelAid/restatis")
```

## Usage

### Authentication

To access the Destatis GENESIS API, you need to have an account at
<https://www-genesis.destatis.de> and store your username and password
for use in R with `gen_auth_save()` (see `?gen_auth_save` for more
details).

### Main features

restatis provides functions (prefixed with `gen_`) for finding,
exploring, and retrieving data from the GENESIS API. See the [“Basic
restatis workflow”
vignette](https://correlaid.github.io/restatis/articles/restatis.html)
for an overview of the main features of the package.

### Caching

restatis uses [memoisation](https://github.com/r-lib/memoise) to cache
query results. This means that if you call a function multiple times
with the same input, the values returned the first time are stored and
reused from the second time.

Cached objects are stored in the memory and do not persist across R
sessions.

## Disclaimer

This package is in no way affiliated with the German Federal Statistical
Office (Destatis). It is a simple wrapper providing R functions to
access Destatis’ API. The package authors are in no way responsible for
the data that can be retrieved using its functions. The license of this
package solely applies to its source code.
