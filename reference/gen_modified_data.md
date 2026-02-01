# gen_modified_data

Function to check for updates, changes, or new objects based on a
specific date.

## Usage

``` r
gen_modified_data(
  code = "",
  database = c("all", "genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st"),
  type = c("all", "tables", "statistics", "statisticsUpdates"),
  date = c("now", "week_before", "month_before", "year_before"),
  credential_list = NULL,
  pagelength = 500,
  verbose = TRUE,
  ...
)
```

## Arguments

- code:

  A character string with a maximum length of 15 characters. Code from a
  database object. Only one code per iteration.

- database:

  Character string. Indicator if the GENESIS ('genesis'), Zensus 2022
  ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de
  ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de
  ('bildung') or genesis.sachsen-anhalt.de ('st') database is called. If
  all databases should be checked, use 'all'. Default option is 'all'.

- type:

  Character string. Specific GENESIS and regionalstatistik.de object
  types: 'tables', 'statistics', and 'statisticsUpdates'. Specific
  Zensus 2022 object types: 'tables' and 'statistics'. All types that
  are specific for one database can be used together through 'all',
  which is the default.

- date:

  Character string. Specific date that is used as the last update or
  upload time to include an object in return. Default option is 'now',
  which uses the current date of your system. Alternative options are
  'week_before', using the current date of your system minus 7 days,
  'month_before', using the current date of your system minus 4 weeks,
  and 'year_before', using the current date of your system minus 52
  weeks. Additionally, it is possible to fill in a specific date of
  format 'DD.MM.YYYY'.

- credential_list:

  A list containing the credentials for the databases to be accessed. If
  'NULL' (default), the function will use the stored credentials from
  [`gen_auth_get()`](https://correlaid.github.io/restatis/reference/gen_auth_get.md).

- pagelength:

  Integer. Maximum length of results or objects (e.g., number of
  tables). Defaults to 500. Maximum of the databases is 25,000 objects.

- verbose:

  Boolean. Indicator if the output of the function should include
  detailed messages and warnings. Default option is 'TRUE'. Set the
  parameter to 'FALSE' to suppress additional messages and warnings.

- ...:

  Additional parameters for the API call. These parameters are only
  affecting the call itself, no further processing. For more details see
  [`vignette("additional_parameter")`](https://correlaid.github.io/restatis/articles/additional_parameter.md).

## Value

A list with all recalled elements from the API. Always includes the code
of the object, the title, and the type of the object. This is done to
facilitate further processing with the data. Attributes are added to the
data.frame describing the search configuration for the returned output.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find objects which were modified last_week
object <- gen_modified_data(date = "week_before")

# Find tables that were new from 31.03.2020
object <- gen_modified_data(type = "tables", date = "31.03.2020")

# Find objects related to the topic "Bevoelkerung" (Code: '12*') which were new today
object <- gen_modified_data(code = "12*")
} # }
```
