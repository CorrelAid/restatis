
# restatis <img src="man/figures/hexsticker_restatis.png" width="160px" align="right" />

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/restatis)](https://CRAN.R-project.org/package=restatis)
[![R-CMD-check](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CorrelAid/restatis/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/CorrelAid/restatis/graph/badge.svg)](https://app.codecov.io/gh/CorrelAid/restatis)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/sindresorhus/awesome)
<!-- badges: end -->

**{restatis}** is a wrapper around the RESTful APIs that provide access
to the three main databases of German official statistics:

- The [**GENESIS database** of the Federal Statistical Office of Germany
  (Destatis)](https://www-genesis.destatis.de/genesis/online),
- [**regionalstatistik.de** which is the database of the German Länder
  (Regionaldatenbank)](https://www.regionalstatistik.de/genesis/online/),
- The [database of the **German 2022 Census** (Zensus
  2022)](https://ergebnisse.zensus2022.de/datenbank/online/).

Almost all functions work on either one of them, on all of them or just
on a selection.

#### Current (known) performance issues

- The function `gen_download_job` currently does not work in the CRAN
  release version for the GENESIS database. This is because the mode of
  downloading the tables has changed. A fix has been deployed to the
  development version and will be coming with the next CRAN update.
- It might be that the creation of jobs with `gen_table` is currently
  buggy in the release version (jobs not being created despite of
  `job = TRUE`). A fix has been deployed to the development version and
  will be coming with the next CRAN update.

## Installation

You can install the released version of `{restatis}` from CRAN:

``` r
install.packages("restatis")
```

Or install a development version of `{restatis}` from
[GitHub](https://github.com/CorrelAid/restatis) with:

``` r
# install.packages("devtools")
devtools::install_github("CorrelAid/restatis")
```

## Usage

### Authentication

To access each one of the APIs, you need to have an account that you can
create on the homepage (see links to them above) and store your username
and password for use in R with `restatis::gen_auth_save()` (see
`?gen_auth_save` for more details).

**Note:** The GENESIS and Zensus 2022 databases do support
authentication with an API token as well. You can set the token as
credential by using setting the parameter `use_token = TRUE` for
`restatis::gen_auth_save()`. The token itself can be found when logging
into the respective webpage with your account and by clicking on
*Webservice (API)* (EN) or *Webservice-Schnittstelle (API)* (DE) in the
bottom left corner. **Important:** The GENESIS database will not let you
create jobs when using API tokens to authenticate. This is why
`{restatis}` will check your credential type once you set `job = TRUE`
for `gen_table()` and error in case a token is used. To enable the use
of jobs, use `gen_auth_save()` and input your username and password (by
setting `use_token = FALSE`).

### Main features

{restatis} provides functions (prefixed with `gen_`) for finding,
exploring, and retrieving data from the three supported APIs. See the
[“Basic restatis workflow”
vignette](https://correlaid.github.io/restatis/articles/restatis.html)
for an overview of the main features of the package.

In short, there are functions divided in two main parts, searching for
(meta)data and retrieving data:

#### Searching for (meta)data

- **gen_catalogue()**: Search the API’s catalogue of data
- **gen_find()**: Find objects related to a search term
- **gen_metadata()**: Find meta data to an objects
- **gen_alternative_terms()**: Find alternative terms to a search term
- **gen_modified_data()**: Find out when an object has last been
  modified
- **gen_objects2stat()**, **gen_objects2var()**, **gen_var2stat()**,
  **gen_val2var()**, **gen_val2var2stat()** and **gen_search_vars()**:
  Find objects, statistics, variables and values related to each other

#### Retrieving data

- **gen_cube()**: Using this function, you can download ‘cube’ objects
- **gen_table()**: Using this function, you can download ‘table’ objects
- **gen_list_jobs()** and **gen_download_job()**: Using this function,
  you can find and download previously created jobs (large tables)

#### Other functions

- **gen_logincheck()**: Perform a logincheck to test your credentials
- **gen_signs()**: Get a list of quality signs (special characters)
  found in the API’s tables
- **gen_update_evas()**: Manually scrape a newer version of the EVAS
  numbers (official statistic IDs)

### Caching

`{restatis}` uses [**memoisation**](https://github.com/r-lib/memoise) to
cache query results. This means that if you call a function multiple
times with the same exact input, the values returned the first time are
stored and reused from the second time on. Cached objects are stored in
memory and do not persist across R sessions. With version 0.3.0, we have
enabled users to turn off caching. The caching option is set to TRUE by
default and can be changed by setting
`options(restatis.use_cache = TRUE)` (or `FALSE`, respectively). You can
get the current state of the option by using
`getOption("restatis.use_cache")`. **Note:** Memoisation is *never* used
for the functions `gen_list_jobs()` and `gen_logincheck()` because there
is no use-case for a cached version of the results of these functions
(e.g., login checks should always be executed when called).

## Disclaimer

This package is in no way affiliated with the German Federal Statistical
Office (Destatis) or the ‘Verbund Statistische Ämter des Bundes und der
Länder’. It is a simple wrapper providing R functions to access
different official statistics APIs. The package authors are in no way
responsible for the data that can be retrieved using its functions and
do not provide support for any problems arising from the APIs’
functionality itself. Conversely, support for problems related to this
package is **exclusively** provided by the package authors. The license
of this package solely applies to its source code.
