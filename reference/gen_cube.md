# gen_cube

Download a cube with data from GENESIS, regionalstatistik.de,
landesdatenbank.nrw.de, bildungsmonitoring.de or
statistikdaten.bayern.de database

## Usage

``` r
gen_cube(
  name,
  database = c("genesis", "regio", "nrw", "bildung"),
  credential_list = NULL,
  area = c("public", "user"),
  values = TRUE,
  metadata = TRUE,
  additionals = FALSE,
  startyear = 1900,
  endyear = 2100,
  timeslices = NULL,
  contents = NULL,
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
  ...
)
```

## Arguments

- name:

  Character string for a cube object (only GENESIS,
  regionalstatistik.de, landesdatenbank.nrw.de or bildungsmonitoring.de)

- database:

  Character string. Indicator if the GENESIS ('genesis'),
  regionalstatistik.de ('regio'), landesdatenbank.nrw.de ('nrw') or
  bildungsmonitoring.de ('bildung') database is called.

- credential_list:

  A list containing the credentials for the databases to be accessed. If
  'NULL' (default), the function will use the stored credentials from
  [`gen_auth_get()`](https://correlaid.github.io/restatis/reference/gen_auth_get.md).

- area:

  Character string. The area in which the table is stored. Possible
  values:

  - `"public"`: cube in the public catalogue

  - `"user"`: cube in the user's account

  - `"all"`: both of the above

- values:

  Boolean. Should values be included?

- metadata:

  Boolean. Should metadata be included?

- additionals:

  Boolean. Should additional metadata be included?

- startyear:

  Four-digit integer. Only retrieve data from this year onward.

- endyear:

  Four-digit integer. Only retrieve data up to this year.

- timeslices:

  Integer. Number of timeslices (cumulative to `startyear` or
  `endyear`).

- contents:

  Character string. Names of required statistical specifications.

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

- ...:

  Additional parameters for the API call (see respective API
  documentation). A valid specification of these will not be checked by
  the function, so wrongful specification may lead to errors.

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
