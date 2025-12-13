# gen_table

Download a table with data from GENESIS, Zensus 2022 or
regionalstatistik.de databases

## Usage

``` r
gen_table(name, ...)
```

## Arguments

- name:

  Character string. Name/code of the table. Use of wildcards (`*`) is
  possible.

- ...:

  Optional parameters passed on to the Genesis API call:

  `area`

  :   Character string. The area in which the table is stored. Possible
      values:

      - `"public"`: table in the public catalogue

      - `"user"`: table in the user's account

      - `"all"`: both of the above

  `compress`

  :   Boolean. Should empty rows and columns be discarded?

  `transpose`

  :   Boolean. Reshape the table between "wide" and "long" format.

  `startyear,endyear`

  :   Four-digit integers. Only retrieve data between these years.

  `regionalvariable`

  :   Character string. Code of the regional variable whose value is
      specified in `regionalkey` to filter the results.

  `regionalkey`

  :   Character string. One or more regional keys. Multiple values can
      be supplied as a character vector or as a single string, with the
      regional keys separated by commas. Use of wildcard (`*`) allowed.

  `classifyingvariable1,classifyingvariable2 ,classifyingvariable3`

  :   Character string. Code of the subject classification (SK-Merkmal)
      to which the selection by means of the corresponding
      `classifyingkey` parameter is to be applied.

  `classifyingkey1,classifyingkey2,classifyingkey3`

  :   Character string. One or more values of a subject classification
      (e.g. "WZ93012"). Applied to the corresponding
      `classifyingvariable` parameter. Multiple keys can be supplied as
      a character vector or as a single string, with the keys separated
      by commas. Use of wildcard (`*`) allowed.

  `stand`

  :   Character string, format: `"DD.MM.YYYY"`. Only retrieve data
      updated after this date.

  `language`

  :   Search terms, returned messages and data descriptions in German
      (`"de"`) or English (`"en"`)?

  `job`

  :   Boolean. Indicate as to whether a job should be created (not
      available with the 'Zensus' database). In order to set job = TRUE
      you have to have username and password saved with gen_auth_save(),
      using API tokens with job = TRUE will result in an error.

  `all_character`

  :   Boolean. Should all variables be imported as 'character'
      variables? Avoids fuzzy data type conversions if there are leading
      zeros or other special characters. Defaults to TRUE.

  `...`

  :   Additional parameters for the API call (see respective API
      documentation). A valid specification of these will not be checked
      by the function, so wrongful specification may lead to errors.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Examples

``` r
if (FALSE) { # \dontrun{
gen_table("21311-0001", database = "genesis")
} # }
```
