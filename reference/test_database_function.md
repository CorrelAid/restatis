# test_database_function

test_database_function

## Usage

``` r
test_database_function(input, credential_list, error.input, text)
```

## Arguments

- input:

  Input to test for database name

- credential_list:

  A list containing the credentials for the databases to be accessed. If
  'NULL' (default), the function will use the stored credentials from
  [`gen_auth_get()`](https://correlaid.github.io/restatis/reference/gen_auth_get.md).

- error.input:

  Indicator error.ignore

- text:

  Indicator verbose
