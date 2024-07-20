#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("xy_variable1", {
  test_that("gen_objects2var does return a list as result", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_objects2var(code = "DLAND", database = "genesis")

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

    expect_s3_class(gen_objects2var(code = "DLAND",
                                    category = "tables",
                                    database = "genesis"),
                    class = "data.frame")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on multiple codes", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = c("DLAND", "LAND"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_objects2var function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on wrong categories", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = "DLAND", detailed = TRUE, category = "variables", database = "genesis"),
    regexp = "Available categories are 'tables', 'statistics', and 'cubes'.")

})

test_that("gen_objects2var function errors on too many categories", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = "611*", detailed = TRUE,
                              category = c("variables", "statistics", "tables", "cubes")),
    regexp = "Parameter 'category' has to have a length of 1 to 3.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on numeric detailed param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = "DLAND", detailed = 1, category = "tables"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")

})

with_mock_dir("xy_variable3", {
  test_that("gen_objects2var function messages on FALSE detailed param", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      gen_objects2var(code = "DLAND", detailed = FALSE, category = "tables", database = "genesis"),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")

  })

})

#-------------------------------------------------------------------------------

test_that("gen_objects2var function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2var(code = "7DLAND", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
