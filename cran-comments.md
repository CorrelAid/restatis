## Resubmission

### Documentation issues

> Please reduce the length of the title to less than 65 characters.

> Please always write package names, software names and API (application programming interface) names in single quotes in title and description.
> e.g: --> 'RESTful'
> Please note that package names are case sensitive. 

> Please add a web reference for the API in the form <https:.....> to the description of the DESCRIPTION file with no space after 'https:' and angle brackets for auto-linking.

These have all been fixed.

### Credential storage

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir(). -> R/gen_auth.R

We are not sure we fully understand this. Is writing in the user's home generally not allowed in function bodies, or only in tests? (Examples and vignettes are skipped/manually pre-rendered and thus not affected.)

If the latter, we have changed `gen_auth_path()` to use a private option `restatis:::config_dir`, which can be set to `tempdir()` in tests. This is the same approach used by the rtweet package ([implementation](https://github.com/ropensci/rtweet/blob/4d42cc8b1d5f4c445bb5865181485423c265f3ed/R/auth.R#L408), [test](https://github.com/ropensci/rtweet/blob/4d42cc8b1d5f4c445bb5865181485423c265f3ed/tests/testthat/test-auth.R#L16)). Currently, we do not test authentication, and testing of API responses is done using [mock files](https://enpiar.com/httptest2/reference/with_mock_dir.html), so no credentials are written in the tests and the tests are not affected by this option.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
