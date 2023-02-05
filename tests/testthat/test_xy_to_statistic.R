#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("xy_statistic1", {
  test_that("xy_to_statistic does return a list as result", {

    result <- restatis::xy_to_statistic(code = "61111")

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Category" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })
})

#-------------------------------------------------------------------------------

with_mock_dir("xy_statistic2", {
  test_that("xy_to_statistic does return a data.frame for a single category", {
    expect_s3_class(restatis::xy_to_statistic(code = "61111",
                                          category = "tables"),
                class = "data.frame")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------


test_that("xy_to_statistic function errors on multiple codes", {
  expect_error(
    restatis::xy_to_statistic(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")
})

test_that("xy_to_statistic function errors on numeric code param", {
  expect_error(
    restatis::xy_to_statistic(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character' or NULL.")
})

#-------------------------------------------------------------------------------

test_that("xy_to_statistic function errors on wrong categories", {
  expect_error(
    restatis::xy_to_statistic(code = "611*", detailed = TRUE, category = "statistics"),
    regexp = "Available categories are tables, variables, and cubes.")
})

test_that("xy_to_statistic function errors on too many categories", {
  expect_error(
    restatis::xy_to_statistic(code = "611*", detailed = TRUE,
                        category = c("variables", "statistics", "tables", "cubes")),
    regexp = "Parameter 'category' has to have a length of 1 to 3.")
})

#-------------------------------------------------------------------------------

test_that("xy_to_statistic function errors on numeric detailed param", {
  expect_error(
    restatis::xy_to_statistic(code = "711*", detailed = 1, category = "tables"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")
})

with_mock_dir("xy_statistic3", {
  test_that("xy_to_statistic function messages on FALSE detailed param", {
    expect_message(
      restatis::xy_to_statistic(code = "61111", detailed = FALSE, category = "tables"),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")
  })
})

#-------------------------------------------------------------------------------

test_that("xy_to_statistic function errors on wrong sort param", {
  expect_error(
    restatis::xy_to_statistic(code = "61111", sortcriterion = "date"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")
})

test_that("xy_to_statistic function errors on wrong sort param type", {
  expect_error(
    restatis::xy_to_statistic(code = "6111*", sortcriterion = 123),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("xy_to_statistic function errors on wrong error.ignore param", {
  expect_error(
    restatis::xy_to_statistic(code = "711*", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})
