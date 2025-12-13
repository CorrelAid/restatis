# set_credentials_auth

set_credentials_auth

## Usage

``` r
set_credentials_auth(path, sys_env, ui_menu_database, use_token)
```

## Arguments

- path:

  Path for the .rds file

- sys_env:

  System environment variable name for the key

- ui_menu_database:

  The database for the auth request ('GENESIS' or 'Zensus 2022')

- use_token:

  Boolean. Do you want to (if possible) set an API token instead of
  password + username? Defaults to FALSE.
