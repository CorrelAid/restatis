#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("variables1", {
  test_that("gen_var2stat function returns list", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_var2stat(code = "61111",
                           detailed = TRUE,
                           sortcriterion = "code",
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

with_mock_dir("variables2_fake", {

  test_that("gen_var2stat function errors if there is a problem (fake response)", {

    # Here, it is necessary to change the mockfile:
    # Change the Status$Code to, e.g., 999
    # Change the Status$Content to contain "test error message"

    skip_on_cran()
    skip_on_ci()

    expect_error(object = gen_var2stat(code = "74111", database = "genesis"),
                 regexp = "test error message")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on multiple codes", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_var2stat function errors on numeric code param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on numeric detailed param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = "711*", detailed = 1, category = "tables"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")

})

with_mock_dir("variables3", {
  test_that("gen_var2stat function messages on FALSE detailed param", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      gen_var2stat(code = "61111", detailed = FALSE, category = "tables", database = "genesis"),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")

  })

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on wrong sort param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = "61111", sortcriterion = "date"),
    regexp = "Parameter 'sortcriterion' has to be 'code' or 'content'.")

})

test_that("gen_var2stat function errors on wrong sort param type", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = "6111*", sortcriterion = 123),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_var2stat function errors on wrong error.ignore param", {

  skip_on_cran()
  skip_on_ci()

  expect_error(
    gen_var2stat(code = "711*", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
