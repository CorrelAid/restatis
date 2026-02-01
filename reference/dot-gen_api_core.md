# Basic API request function

(Uncached) Low-level function to interact with the one of the APIs

## Usage

``` r
.gen_api_core(endpoint, database, credential_list = NULL, ...)
```

## Arguments

- endpoint:

  Character string. The endpoint of the API that is to be queried.

- database:

  The database the query should be sent to.

- credential_list:

  A named list including username and password for the database(s) to be
  queried. If NULL (default), the function will try to get credentials
  from the environment variables.

- ...:

  Further parameters passed on to the final API call.
