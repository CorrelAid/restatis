# logincheck_stop_or_warn

logincheck_stop_or_warn

## Usage

``` r
logincheck_stop_or_warn(response, error, verbose, database)
```

## Arguments

- response:

  A HTTP response object

- error:

  Boolean. Should the function warn or throw an error?

- verbose:

  Boolean. Should the function message in case of success?

- database:

  The database that the check should be run for

## Value

In case of failure warns or errors. Invisibly returns TRUE (success) or
FALSE (failure)
