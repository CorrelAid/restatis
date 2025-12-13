# Find similar search terms

Function to find search terms that are similar or related to one another
in spelling and also represented in the databases. Important note: The
API call is searching for terms with the same characters. To be useful
in searching for related terms it is highly recommended to work with
"\*" placeholders (see examples). The placeholder can be placed before
and/or after the search term.

## Usage

``` r
gen_alternative_terms(
  term = NULL,
  similarity = TRUE,
  database = c("all", "genesis", "zensus", "regio", "bayern", "nrw", "bildung", "sa"),
  pagelength = 500,
  verbose = TRUE,
  ...
)
```

## Arguments

- term:

  Character string. Maximum length of 15 characters. Term or word for
  which you are searching for alternative or related terms. Use of '\*'
  as a placeholder is possible to generate broader search areas.

- similarity:

  Boolean. Indicator if the output of the function should be sorted
  based on a Levenshtein edit distance based on the
  [`adist()`](https://rdrr.io/r/utils/adist.html) function. Default is
  'TRUE'.

- database:

  Character string. Indicator if the GENESIS ('genesis'), Zensus 2022
  ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de
  ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de
  ('bildung') or genesis.sachsen-anhalt.de ('sa') database is called. If
  all databases should be checked, use 'all'. Default option is 'all'.

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

A list with all recollected elements from the respective database.
Attributes are added to the data.frame, describing the search
configuration for the returned output.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find terms at GENESIS that are the same (in spelling) to search term "bus"
# and sort them by Levenshtein edit distance
object <- gen_alternative_terms(term = "bus", similarity = TRUE, database = "genesis")

# Find terms at GENESIS that are related (in spelling) to search term "bus"
object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "genesis")

# Find terms at Zensus 2022 that are related (in spelling) to search term "wohn"
object <- gen_alternative_terms(term = "wohn*", similarity = TRUE, database = "zensus")
} # }
```
