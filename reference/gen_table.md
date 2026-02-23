# Download Table As Data.Frame

Download a table with data from one of the supported databases. This is
the main function to fetch data, apart from `gen_cube`.

## Usage

``` r
gen_table(
  name,
  database = c("genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st"),
  credential_list = NULL,
  area = c("all", "public", "user"),
  compress = FALSE,
  transpose = FALSE,
  startyear = 1900,
  endyear = 2100,
  regionalvariable = NULL,
  regionalkey = NULL,
  classifyingvariable1 = NULL,
  classifyingkey1 = NULL,
  classifyingvariable2 = NULL,
  classifyingkey2 = NULL,
  classifyingvariable3 = NULL,
  classifyingkey3 = NULL,
  stand = NULL,
  language = Sys.getenv("RESTATIS_LANG"),
  job = FALSE,
  all_character = TRUE,
  ...
)
```

## Arguments

- name:

  Character string. Name/code of the table. Use of wildcards (`*`) is
  possible.

- database:

  Character string. Indicator if the GENESIS ('genesis'), Zensus 2022
  ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de
  ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de
  ('bildung') or genesis.sachsen-anhalt.de ('st') database is called.

- credential_list:

  A list containing the credentials for the databases to be accessed. If
  'NULL' (default), the function will use the stored credentials from
  [`gen_auth_get()`](https://correlaid.github.io/restatis/reference/gen_auth_get.md).

- area:

  Character string. The area in which the table is stored. Possible
  values:

  - `"public"`: table in the public catalogue

  - `"user"`: table in the user's account

  - `"all"`: both of the above

- compress:

  Boolean. Should empty rows and columns be discarded?

- transpose:

  Boolean. Reshape the table between `"wide"` and `"long"` format.

- startyear:

  Four-digit integer. Only retrieve data from this year onward.

- endyear:

  Four-digit integer. Only retrieve data up to this year.

- regionalvariable:

  Character string. Code of the regional variable whose value is
  specified in `regionalkey` to filter the results.

- regionalkey:

  Character string. One or more regional keys. Multiple values can be
  supplied as a character vector or as a single string, with the
  regional keys separated by commas. Use of wildcard (`*`) allowed.

- classifyingvariable1:

  Character string. Code of the subject classification (SK-Merkmal) to
  which the selection by means of `classifyingkey1` is applied.

- classifyingkey1:

  Character string. One or more values of a subject classification (e.g.
  `"WZ93012"`). Applied to `classifyingvariable1`. Multiple keys can be
  supplied as a character vector or as a single string, with the keys
  separated by commas. Use of wildcard (`*`) allowed.

- classifyingvariable2:

  Character string. Code of the subject classification (SK-Merkmal) to
  which the selection by means of `classifyingkey2` is applied.

- classifyingkey2:

  Character string. One or more values of a subject classification.
  Applied to `classifyingvariable2`. Multiple keys can be supplied as a
  character vector or as a single string, with the keys separated by
  commas. Use of wildcard (`*`) allowed.

- classifyingvariable3:

  Character string. Code of the subject classification (SK-Merkmal) to
  which the selection by means of `classifyingkey3` is applied.

- classifyingkey3:

  Character string. One or more values of a subject classification.
  Applied to `classifyingvariable3`. Multiple keys can be supplied as a
  character vector or as a single string, with the keys separated by
  commas. Use of wildcard (`*`) allowed.

- stand:

  Character string, format `"DD.MM.YYYY"`. Only retrieve data updated
  after this date.

- language:

  Character string. Search terms, returned messages and data
  descriptions in German (`"de"`) or English (`"en"`).

- job:

  Boolean. Indicate whether a job should be created (not available with
  the `"Zensus"` database). To set `job = TRUE`, username and password
  must be saved with
  [`gen_auth_save()`](https://correlaid.github.io/restatis/reference/gen_auth_save.md);
  using API tokens with `job = TRUE` will result in an error.

- all_character:

  Boolean. Should all variables be imported as character variables?
  Avoids fuzzy data type conversions if there are leading zeros or other
  special characters. Defaults to `TRUE`.

- ...:

  Additional parameters for the API call (see respective API
  documentation). A valid specification of these will not be checked by
  the function, so wrongful specification may lead to errors.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Examples

``` r
if (FALSE) { # \dontrun{
gen_table("21311-0001", database = "genesis")
} # }
```
