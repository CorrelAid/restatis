#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("meta1", {

  test_that("gen_metadata returns list", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_metadata(code = "11111-0001",
                           category = "table",
                           database = "genesis")

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Code" %in% names(attrs))
    expect_true("Method" %in% names(attrs))
    expect_true("Updated" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------

with_mock_dir("meta2_fake", {

  test_that("gen_metadata errors if there is an error code (fake response)", {

    # Here, the mockfile needs to be altered:
    # Status$Code = 999
    # Status$Content = "test error message"

    skip_on_cran()
    skip_on_ci()

    expect_error(object = gen_metadata(code = "1*",
                                       category = "cube",
                                       database = "genesis"),
                 regexp = "test error message")

  })

})

#-------------------------------------------------------------------------------

with_mock_dir("meta3", {

  test_that("gen_metadata gives message if error.ignore = TRUE", {

    skip_on_cran()
    skip_on_ci()

    expect_message(object = gen_metadata(code = "11111-0001",
                                         category = "table",
                                         error.ignore = TRUE,
                                         database = "genesis"),
                   regexp = "Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_metadata function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(object = gen_metadata(code = 12345,
                                     category = "Table",
                                     database = "genesis"),
               regexp = "Parameter 'code' has to be of type 'character'.")

})

test_that("gen_metadata function errors on multiple categories", {

  skip_on_cran()
  skip_on_ci()

  expect_error(object = gen_metadata(code = "12345",
                                     category = c("Table", "Cube"),
                                     database = "genesis"),
               regexp = "Parameter 'category' must have a length of 1. Please specify the category.")

})

test_that("gen_metadata function errors on wrong category", {

  skip_on_cran()
  skip_on_ci()

  expect_error(object = gen_metadata(code = "11111",
                                     category = "Table",
                                     database = "genesis"),
               regexp = "Available categories for parameter 'category' for 'genesis' database are 'cube', 'table', 'statistic', 'variable', and 'value'")

})

test_that("gen_metadata function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(object = gen_metadata(code = "11111",
                                     error.ignore = 1,
                                     category = "table",
                                     database = "genesis"),
               regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
