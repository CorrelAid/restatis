#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("searchvars1", {
  test_that("gen_search_vars returns a list element", {

  skip_on_cran()
  skip_on_ci()

  result <- gen_search_vars(code = NULL,
                            sortcriterion = c("code", "content"),
                            error.ignore = FALSE,
                            database = "genesis")

  expect_type(result, type = "list")

  attrs <- attributes(result)

  expect_true("Code" %in% names(attrs))
  expect_true("Language" %in% names(attrs))
  expect_true("Pagelength" %in% names(attrs))
  expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_search_vars function errors on multiple codes", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_search_vars(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_search_vars function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_search_vars(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_search_vars function errors on wrong sort param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_search_vars(code = "61111", sortcriterion = "date"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")

})

test_that("gen_search_vars function errors on wrong sort param type", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_search_vars(code = "6111*", sortcriterion = 123),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_search_vars function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_search_vars(code = "711*", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})

