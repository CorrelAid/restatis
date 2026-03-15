#-------------------------------------------------------------------------------
# Tests related to API calls using 'credential_list'
#-------------------------------------------------------------------------------

test_that("functions error correctly on erroneous credential_list parameter values", {

  #-----------------------------------------------------------------------------

  expect_error(object = result <- gen_catalogue(code = "12*",
                                                database = "genesis",
                                                verbose = FALSE,
                                                credential_list = list(regio = c(username = "bar",
                                                                                 password = "foo"))),
               regexp = "None of the databases you defined in 'database' are contained in your 'credential_list'. Please re-specify the credentials.")

  expect_error(object = result <- gen_catalogue(code = "12*",
                                                database = c("genesis", "zensus"),
                                                category = "tables",
                                                verbose = FALSE,
                                                credential_list = list(regio = c(username = "bar", password = "foo"),
                                                                       nrw = c(username = "foo", password = "bar"),
                                                                       bayern = c(username = "foo", password = "bar"))),
               regexp = "None of the databases you defined in 'database' are contained in your 'credential_list'. Please re-specify the credentials.")

  expect_error(object = result <- gen_catalogue(code = "12*",
                                                database = c("genesis", "zensus"),
                                                category = "tables",
                                                verbose = FALSE,
                                                credential_list = list(genesis = c(username = "bar", password = "foo"),
                                                                       zensus = c(username = "foo", password = "bar"))),
               regexp = "No json-csv file detected.")

  expect_error(object = result <- gen_catalogue(code = "12*",
                                                database = c("genesis", "regio"),
                                                verbose = FALSE,
                                                credential_list = list(regio = c(username = "bar", username = "foo"),
                                                                       genesis = c(username = "foo", username = "bar"))),
               regexp = "Every database that is requested in the parameter 'database' needs its own list entry including the entries")

  #-----------------------------------------------------------------------------

  expect_error(object = result <- gen_catalogue(code = "12*",
                                                database = c("genesis", "zensus"),
                                                category = "tables",
                                                verbose = FALSE,
                                                credential_list = list(genesis = c(username = "foo", password = "bar"))),
               regexp = "Not all databases you defined in 'database' are contained in your 'credential_list'")

})

#-------------------------------------------------------------------------------

test_that("functions messages correctly on special credential_list parameter values", {

  skip_on_cran()
  skip_on_ci()

  #-----------------------------------------------------------------------------

  expect_message(object = expect_error(object = gen_catalogue(code = "12*",
                                                              database = "genesis",
                                                              credential_list = list(genesis = c(username = "bar",
                                                                                                 password = "foo"))),
                                       regexp = "No json-csv file detected"),
                 regexp = "can be a potential security threat")

  #-----------------------------------------------------------------------------

  expect_message(object = expect_error(object = result <- gen_catalogue(code = "12*",
                                                                        database = c("genesis", "zensus"),
                                                                        category = "tables",
                                                                        error.ignore = TRUE,
                                                                        verbose = FALSE,
                                                                        credential_list = list(genesis = c(username = "foo",
                                                                                                           password = "bar"))),
                                       regexp = "No json-csv file detected."),
                 regexp = "Functions continues with those available")

})

#-------------------------------------------------------------------------------

test_that("restatis functions respond correctly to ill-defined credential_list", {

  expect_error(object = gen_objects2var(code = "7DLAND",
                                        database = "genesis",
                                        credential_list = c("genesis" = "foobar")),
               regex = "Parameter 'credential_list' has to be of type 'list' if 'credential_type' is set.")

  expect_error(object = gen_objects2var(code = "7DLAND",
                                        database = "genesis",
                                        credential_list = list("regio" = c("password" = "foo",
                                                                           "username" = "bar"))),
               regex = "None of the databases you defined in 'database' are contained in your 'credential_list'. Please re-specify the credentials.")

})
