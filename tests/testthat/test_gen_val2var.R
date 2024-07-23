#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("values1", {
  test_that("gen_val2var function errors if there is no result", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      gen_val2var(code = "61111",
                  detailed = TRUE,
                  sortcriterion = "code",
                  database = "genesis",
                  language = "en"),
      regexp = "No objects found.")

  })

})

# #-------------------------------------------------------------------------------

with_mock_dir("values2", {
  test_that("gen_val2var function returns list", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_val2var(code = "DLAND",
                                    detailed = TRUE,
                                    sortcriterion = "code",
                                    database = "genesis")

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Name" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_val2var function errors on multiple codes", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_val2var(code = c("611*", "711*"), detailed = TRUE, category = "tables", database = "genesis"),
    regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_val2var function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_val2var(code = 12345, detailed = TRUE, category = "tables", "genesis"),
    regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_val2var function errors on wrong sort param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_val2var(code = "61111", sortcriterion = "date", database = "genesis"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")

})

test_that("gen_val2var function errors on wrong sort param type", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_val2var(code = "6111*", sortcriterion = 123, database = "genesis"),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_val2var function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_val2var(code = "711*", error.ignore = 1, database = "genesis"),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})

