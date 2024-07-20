#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("xy_statistic1", {
  test_that("gen_objects2stat does return a list as result", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_objects2stat(code = "61111", database = "genesis")

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
  test_that("gen_objects2stat does return a data.frame for a single category", {

    skip_on_cran()
    skip_on_ci()

    expect_s3_class(gen_objects2stat(code = "61111",
                                     category = "tables",
                                     database = "genesis"),
                class = "data.frame")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on multiple codes", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = c("611*", "711*"), detailed = TRUE, category = "tables", database = "genesis"),
    regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_objects2stat function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = 12345, detailed = TRUE, category = "tables", database = "genesis"),
    regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on wrong categories", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = "611*", detailed = TRUE, category = "statistics", database = "genesis"),
    regexp = "Available categories are 'tables', 'variables', and 'cubes'.")

})

test_that("gen_objects2stat function errors on too many categories", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = "611*", detailed = TRUE,
                        category = c("variables", "statistics", "tables", "cubes"), database = "genesis"),
    regexp = "Parameter 'category' has to have a length of 1 to 3.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on numeric detailed param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = "711*", detailed = 1, category = "tables", database = "genesis"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")

})

with_mock_dir("xy_statistic3", {
  test_that("gen_objects2stat function messages on FALSE detailed param", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      gen_objects2stat(code = "61111", detailed = FALSE, category = "tables", database = "genesis"),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")

  })

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_objects2stat(code = "711*", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
