#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("modified1", {
  test_that("gen_modified_data function returns list", {

    skip_on_cran()
    skip_on_ci()

    expect_type(
      restatis::gen_modified_data(code = "61111", date = "01.01.2022"),
      type = "list")
  })
})

with_mock_dir("modified2", {
  test_that("gen_modified_data function returns message", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      restatis::gen_modified_data(code = "61111"),
      regexp = "Please note that this date is calculated automatically and may differ
              from manually entered data. Manually entered data must have
              the format DD.MM.YYYY.")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("modified3", {
  test_that("gen_modified_data function returns list with attributes", {

    skip_on_cran()
    skip_on_ci()

    result <- restatis::gen_modified_data(code = "61111", date = "01.01.2022")

    attrs <- attributes(result)

    expect_true("names" %in% names(attrs))
    expect_true("Code" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })
})

with_mock_dir("modified4_fake", {
  test_that("gen_modified_data function warns if there is a non-zero status code", {

    skip_on_cran()
    skip_on_ci()

    expect_warning(
      restatis::gen_modified_data(code = "61234", date = "01.01.2022"),
    regexp = "test warning message")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_modified_data errors on misspecified dates", {
  expect_error(
    restatis::gen_modified_data(code = "61111", date = "1.1.2022"),
  regexp = "it has to be of length 1 and format DD.MM.YYYY")
})

test_that("gen_modified_data errors on misspecified dates", {
  expect_error(
    restatis::gen_modified_data(code = "61111", date = 23456),
  regexp = "'date', it has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("gen_modified_data errors on multiple codes", {
  expect_error(
    restatis::gen_modified_data(code = c("611*", "711*")),
  regexp = "Parameter 'code' must be a single string.")
})

test_that("gen_modified_data function errors on numeric code param", {
  expect_error(
    restatis::gen_modified_data(code = 12345),
  regexp = "Parameter 'code' has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("gen_modified_data function errors on wrong type value", {
  expect_error(
    restatis::gen_modified_data(code = "12345", type = "diesdasananas"),
  regexp = "Available categories for parameter 'type' are 'tables', 'statistics', 'statistic updates', and 'all'.")
})
