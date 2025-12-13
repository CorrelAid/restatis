# Save database credentials

Save credentials of the different databases for further convenient use

## Usage

``` r
gen_auth_save(database, use_token = FALSE)
```

## Arguments

- database:

  Character string. The database to store credentials for ('all',
  'genesis', 'zensus', 'regio', 'bayern', 'nrw', 'bildung' or 'sa').

- use_token:

  Boolean. Do you want to (if possible) set an API token instead of
  password + username? Note: This is not supported by
  regionalstatistik.de. Defaults to FALSE.

## Value

Path to the RDS file in which credentials are saved, invisibly.

## Details

Username and password are encrypted and saved as RDS in the package
config directory. A random string is generated and stored in the session
environment variable `GENESIS_KEY`. This string is used as the key to
encrypt and decrypt the entered credentials. To avoid having to save
authentication in future sessions, `GENESIS_KEY` can be added to
.Renviron. The `{usethis}` package includes a helper function for
editing .Renviron files from an R session with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).

## Examples

``` r
if (FALSE) { # \dontrun{
gen_auth_save("zensus")
} # }
```
