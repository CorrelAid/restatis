# Retrieve database credentials

Function to retrieve the credentials stored via
[`gen_auth_save()`](https://correlaid.github.io/restatis/reference/gen_auth_save.md)

## Usage

``` r
gen_auth_get(database = NULL)
```

## Arguments

- database:

  Character string. The database to get the credentials for ('all',
  'genesis', 'zensus' and 'regio').

## Value

Credentials for the database(s) chosen by the user

## Examples

``` r
if (FALSE) { # \dontrun{
gen_auth_get("all")
} # }
```
