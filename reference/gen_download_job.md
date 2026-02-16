# gen_download_job

gen_download_job

## Usage

``` r
gen_download_job(
  name,
  database = c("genesis", "regio", "bayern", "st", "nrw", "bildung"),
  credential_list = NULL,
  area = c("all", "public", "user"),
  compress = FALSE,
  language = Sys.getenv("RESTATIS_LANG"),
  all_character = TRUE
)
```

## Arguments

- name:

  Character string. The job code retrieved by using gen_list_jobs().

- database:

  Character string. Indicator which database should be called. Only one
  database can be addressed per function call. Default option is
  'genesis'.

- credential_list:

  A list containing the credentials for the databases to be accessed. If
  'NULL' (default), the function will use the stored credentials from
  [`gen_auth_get()`](https://correlaid.github.io/restatis/reference/gen_auth_get.md).

- area:

  Character string. Indicator from which area of the database the
  results are called. In general, 'all' is the appropriate solution.
  Default option is 'all'.

- compress:

  Boolean. Should empty rows and columns be discarded? Default is FALSE.

- language:

  Character string. Defines if the decimal mark and grouping mark of
  integers should be represented based on the European (e.g.: '100,5',
  '200.000,5') or American ('100.5', '200,000.5') system. Defaults to
  'Sys.getenv("RESTATIS_LANG")'.

- all_character:

  Boolean. Should all variables be imported as 'character' variables?
  Avoids fuzzy data type conversions if there are leading zeros or other
  special characters. Defaults to TRUE.

## Value

Returns a data.frame with the table content

## Examples

``` r
if (FALSE) { # \dontrun{
gen_download_job("21311-00-01-1_123456789", "regio")
} # }
```
