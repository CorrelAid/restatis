#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("searchvars1", {
  test_that("search_variables returns a list element", {

  result <- search_variables(code = NULL,
                             sortcriterion = c("code", "content"),
                             error.ignore = FALSE)

  expect_type(result, type = "list")

  attrs <- attributes(result)

  expect_true("Code" %in% names(attrs))
  expect_true("Language" %in% names(attrs))
  expect_true("Pagelength" %in% names(attrs))
  expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------

with_mock_dir("searchvars2", {
  test_that("search_variables returns a list element", {

    expect_error(
      search_variables(code = "74111",
                       sortcriterion = c("code", "content"),
                       error.ignore = FALSE),
      regexp = "test error message")
  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("search_variables function errors on multiple codes", {
  expect_error(
    restatis::search_variables(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")
})

test_that("search_variables function errors on numeric code param", {
  expect_error(
    restatis::search_variables(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character' or NULL.")
})

#-------------------------------------------------------------------------------

test_that("search_variables function errors on wrong sort param", {
  expect_error(
    restatis::search_variables(code = "61111", sortcriterion = "date"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")
})

test_that("search_variables function errors on wrong sort param type", {
  expect_error(
    restatis::search_variables(code = "6111*", sortcriterion = 123),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("search_variables function errors on wrong error.ignore param", {
  expect_error(
    restatis::search_variables(code = "711*", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})

