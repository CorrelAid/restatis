#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

test_that("gen_var2stat function returns list", {

  with_mock_dir("variables1", {

    skip_on_cran()

    result <- gen_var2stat(code = "61111",
                           detailed = TRUE,
                           sortcriterion = "code",
                           database = "genesis")

    expect_type(object = result, type = "list")

    attrs <- attributes(result)

    expect_true("Code" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors if there is a problem (fake response)", {

  with_mock_dir("variables2_fake", {

    skip_on_cran()

    # Here, it is necessary to change the mockfile:
    # Change the Status$Code to, e.g., 999
    # Change the Status$Content to contain "test error message"

    # First download correct file with:
    # expect_type(object = gen_var2stat(code = "74111", database = "genesis"),
    #             type = "list")

    expect_error(object = gen_var2stat(code = "74111",
                                       database = "genesis"),
                 regexp = "test error message")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on multiple codes", {

    expect_error(object = gen_var2stat(code = c("611*", "711*"),
                                       detailed = TRUE,
                                       category = "tables"),
                 regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_var2stat function errors on numeric code param", {

  expect_error(object = gen_var2stat(code = 12345,
                                     detailed = TRUE,
                                     category = "tables"),
               regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on numeric detailed param", {

  expect_error(object = gen_var2stat(code = "711*",
                                     detailed = 1,
                                     category = "tables"),
               regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")

})

test_that("gen_var2stat function messages on FALSE detailed param", {

  with_mock_dir("variables3", {

    skip_on_cran()

    expect_message(object = gen_var2stat(code = "61111",
                                         detailed = FALSE,
                                         category = "tables",
                                         database = "genesis"),
                   regexp = "Use 'detailed = TRUE' to obtain the complete output.")

  })

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on wrong sort param", {

  expect_error(object = gen_var2stat(code = "61111",
                                     sortcriterion = "date"),
               regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")

})

test_that("gen_var2stat function errors on wrong sort param type", {

  expect_error(object = gen_var2stat(code = "6111*",
                                     sortcriterion = 123),
               regexp = "Parameter 'sortcriterion' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on wrong error.ignore param", {

  expect_error(object = gen_var2stat(code = "711*",
                                     error.ignore = 1),
               regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
