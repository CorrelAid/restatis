test_that("functions error correctly on erroneous credential_list parameter values", {

  #-----------------------------------------------------------------------------

  expect_error(

    result <- gen_catalogue(code = "12*",
                            database = "genesis",
                            verbose = FALSE,
                            credential_list = list(regio = c(username = "bar", password = "foo"))),
    regexp = "Not all databases you defined in 'database' are contained in your 'credential_list'."

  )

  expect_error(

    result <- gen_catalogue(code = "12*",
                            database = c("genesis", "zensus"),
                            category = "tables",
                            verbose = FALSE,
                            credential_list = list(regio = c(username = "bar", password = "foo"),
                                                   nrw = c(username = "foo", password = "bar"),
                                                   bayern = c(username = "foo", password = "bar"))),
    regexp = "Not all databases you defined in 'database' are contained in your 'credential_list'"

  )

  expect_error(

    result <- gen_catalogue(code = "12*",
                            database = c("genesis", "zensus"),
                            category = "tables",
                            verbose = FALSE,
                            credential_list = list(genesis = c(username = "bar", password = "foo"),
                                                   zensus = c(username = "foo", password = "bar"))),
    regexp = "No json-csv file detected."

  )

  expect_error(

    result <- gen_catalogue(code = "12*",
                            database = c("genesis", "regio"),
                            verbose = FALSE,
                            credential_list = list(regio = c(username = "bar", username = "foo"),
                                                   genesis = c(username = "foo", username = "bar"))),
    regexp = "Every database that is requested in the parameter 'database' needs its own list entry including the entries"

  )

  #-----------------------------------------------------------------------------

  expect_error(

    result <- gen_catalogue(code = "12*",
                            database = c("genesis", "zensus"),
                            category = "tables",
                            verbose = FALSE,
                            credential_list = list(genesis = c(username = "foo", password = "bar"))),
    regexp = "You have defined more databases in 'database' than you have in your 'credential_list'."

  )

})

#-------------------------------------------------------------------------------

test_that("functions messages correctly on special credential_list parameter values", {

  skip_on_cran()
  skip_on_ci()

  #-----------------------------------------------------------------------------

  expect_message(

    expect_error(

      gen_catalogue(code = "12*",
                    database = "genesis",
                    credential_list = list(genesis = c(username = "bar", password = "foo"))),
      regexp = "No json-csv file detected"

    ),

    regexp = "can be a potential security threat"

  )

  #-----------------------------------------------------------------------------

  expect_message(

    expect_error(

      result <- gen_catalogue(code = "12*",
                              database = c("genesis", "zensus"),
                              category = "tables",
                              error.ignore = TRUE,
                              verbose = FALSE,
                              credential_list = list(genesis = c(username = "foo", password = "bar"))),
      regexp = "No json-csv file detected."

    ),

    regexp = "The function continues with those available"

  )

})
