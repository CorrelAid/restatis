# gen_cube

Download a cube with data from GENESIS or regionalstatistik.de database

## Usage

``` r
gen_cube(name, ...)
```

## Arguments

- name:

  Character string for a cube object (only GENESIS and
  regionalstatistik.de)

- ...:

  Further (optional) parameters passed on to the API call:

  `area`

  :   Character string. The area in which the table is stored. Possible
      values:

      - `"public"`: cube in the public catalogue

      - `"user"`: cube in the user's account

      - `"all"`: both of the above

  `values`

  :   Boolean. Should values be included?

  `metadata`

  :   Boolean. Should metadata be included?

  `additionals`

  :   Boolean. Should additional metadata be included?

  `contents`

  :   Character string. Names of required statistical specifications

  `startyear,endyear`

  :   Four-digit integers. Only retrieve data between these years.

  `timeslices`

  :   Integer. Number of timeslices (cumulative to startyear or endyear)

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

  :   Character string. Search terms, returned messages and data
      descriptions in German (`"de"`) or English (`"en"`)?

  `...`

  :   Additional parameters for the API call (see respective API
      documentation). A valid specification of these will not be checked
      by the function, so wrongful specification may lead to errors.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html). Non-data
contents of the data cube object are saved in the `metadata`
[attribute](https://rdrr.io/r/base/attr.html) of the data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
gen_cube("47414BJ002")
} # }
```
