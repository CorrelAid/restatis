#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("xy_variable1", {
  test_that("gen_objects2var does return a list as result", {

    skip_on_cran()
    skip_on_ci()

    result <- restatis::gen_objects2var(code = "DLAND")

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Category" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })
})

#-------------------------------------------------------------------------------

with_mock_dir("xy_variable2", {
  test_that("gen_objects2var does return a data.frame for a single category", {

    skip_on_cran()
    skip_on_ci()

    expect_s3_class(restatis::gen_objects2var(code = "DLAND",
                                             category = "tables"),
                    class = "data.frame")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on multiple codes", {
  expect_error(
    restatis::gen_objects2var(code = c("DLAND", "LAND"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")
})

test_that("gen_objects2var function errors on numeric code param", {
  expect_error(
    restatis::gen_objects2var(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on wrong categories", {
  expect_error(
    restatis::gen_objects2var(code = "DLAND", detailed = TRUE, category = "variables"),
    regexp = "Available categories are tables, statistics, and cubes.")
})

test_that("gen_objects2var function errors on too many categories", {
  expect_error(
    restatis::gen_objects2var(code = "611*", detailed = TRUE,
                              category = c("variables", "statistics", "tables", "cubes")),
    regexp = "Parameter 'category' has to have a length of 1 to 3.")
})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on numeric detailed param", {
  expect_error(
    restatis::gen_objects2var(code = "DLAND", detailed = 1, category = "tables"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")
})

with_mock_dir("xy_variable3", {
  test_that("gen_objects2var function messages on FALSE detailed param", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      restatis::gen_objects2var(code = "DLAND", detailed = FALSE, category = "tables"),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")
  })
})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on wrong sort param", {
  expect_error(
    restatis::gen_objects2var(code = "DLAND", sortcriterion = "date"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")
})

test_that("gen_objects2var function errors on wrong sort param type", {
  expect_error(
    restatis::gen_objects2var(code = "DLAND", sortcriterion = 123),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")
})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on wrong error.ignore param", {
  expect_error(
    restatis::gen_objects2var(code = "7DLAND", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})
