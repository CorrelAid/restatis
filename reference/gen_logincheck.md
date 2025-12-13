# gen_logincheck

Function to check if a login is possible for a certain database.

## Usage

``` r
gen_logincheck(database = "all", verbose = TRUE, ...)
```

## Arguments

- database:

  Character string. Indicator if the GENESIS ('genesis'), Zensus 2022
  ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de
  ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de
  ('bildung') or genesis.sachsen-anhalt.de ('sa') database is called. If
  all databases should be checked, use 'all'. Default option is 'all'.

- verbose:

  Boolean. In case of success, should a message be printed? Defaults to
  'TRUE'.

- ...:

  Additional parameters for the API call. These parameters are only
  affecting the call itself, no further processing. For more details see
  [`vignette("additional_parameter")`](https://correlaid.github.io/restatis/articles/additional_parameter.md).

## Value

Leads to an informative error message if the login check failed and
returns FALSE invisibly. Invisibly returns TRUE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
gen_logincheck("zensus")
} # }
```
