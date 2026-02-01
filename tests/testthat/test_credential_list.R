#-------------------------------------------------------------------------------
# Tests related to API calls using 'credential_list'
#-------------------------------------------------------------------------------

test_that("restatis functions respond correctly to ill-defined credentials_list", {

  skip_on_cran()
  skip_on_ci()

  expect_error(gen_objects2var(code = "7DLAND",
                               database = "genesis",
                               credential_list = c("genesis" = "foobar")),
               regex = "Parameter 'credential_list' has to be of type list if 'credential_type'.")

  expect_error(gen_objects2var(code = "7DLAND",
                               database = "genesis",
                               credential_list = list("regio" = c("password" = "foo",
                                                                  "username" = "bar"))),
               regex = "The following databases are not accessible to you")

})
