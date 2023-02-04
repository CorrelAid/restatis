#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("modified1", {
  test_that("modified_data function returns list", {
    expect_type(
      restatis::modified_data(code = "61111", date = "01.01.2022"),
      type = "list")
  })
})

with_mock_dir("modified2", {
  test_that("modified_data function returns message", {
    expect_message(
      restatis::modified_data(code = "61111"),
      regexp = "Please note that this date is calculated automatically")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("modified3", {
  test_that("modified_data function returns list with attributes", {

      result <- restatis::modified_data(code = "61111", date = "01.01.2022")

      attrs <- attributes(result)

      expect_true("names" %in% names(attrs))
      expect_true("Code" %in% names(attrs))
      expect_true("Language" %in% names(attrs))
      expect_true("Pagelength" %in% names(attrs))
      expect_true("Copyright" %in% names(attrs))

  })
})

with_mock_dir("modified4", {
  test_that("modified_data function warns if there is a non-zero status code", {
    expect_warning(
      restatis::modified_data(code = "61234", date = "01.01.2022"),
    regexp = "test warning message")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("modified_data errors on misspecified dates", {
  expect_error(
    restatis::modified_data(code = "61111", date = "1.1.2022"),
  regexp = "it has to be of length 1 and format DD.MM.YYYY")
})

test_that("modified_data errors on misspecified dates", {
  expect_error(
    restatis::modified_data(code = "61111", date = 23456),
  regexp = "'date', it has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("modified_data errors on multiple codes", {
  expect_error(
    restatis::modified_data(code = c("611*", "711*")),
  regexp = "Parameter 'code' must be a single string.")
})

test_that("modified_data function errors on numeric code param", {
  expect_error(
    restatis::modified_data(code = 12345),
  regexp = "Parameter 'code' has to be of type 'character' or NULL.")
})

#-------------------------------------------------------------------------------

test_that("modified_data function errors on wrong type value", {
  expect_error(
    restatis::modified_data(code = "12345", type = "diesdasananas"),
  regexp = "Available categories for parameter 'type' are 'tables', 'statistics', 'statistic updates', and 'all'.")
})

