#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("find1", {
  test_that("gen_find returns list", {

    skip_on_cran()
    skip_on_ci()

    result <- restatis::gen_find(term = "forst",
                                 detailed = TRUE,
                                 ordering = TRUE,
                                 error.ignore = FALSE)

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Term" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("find2_fake", {
  test_that("gen_find errors if there is an error code", {

    skip_on_cran()
    skip_on_ci()

    expect_error(
      restatis::gen_find(term = "bus", error.ignore = TRUE),
      regexp = "test error message")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("find3", {
  test_that("gen_find messages for 'detailed = TRUE'", {

    skip_on_cran()
    skip_on_ci()

    expect_message(
      restatis::gen_find(term = "zensus", error.ignore = TRUE),
      regexp = "Use 'detailed = TRUE' to obtain the complete output.")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_find function errors on numeric term param", {
  expect_error(
    restatis::gen_find(term = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'term' has to be of type 'character'.")
})

test_that("gen_find function errors on wrong category", {
  expect_error(
    restatis::gen_find(term = "bus", detailed = TRUE, category = "table"),
    regexp = "Available categories are all, tables, statistics, variables, and cubes.")
})

test_that("gen_find function errors on wrong detailed param", {
  expect_error(
    restatis::gen_find(term = "bus", detailed = 1, category = "tables"),
    regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")
})

test_that("gen_find function errors on wrong ordering param", {
  expect_error(
    restatis::gen_find(term = "bus", ordering = 1, category = "tables"),
    regexp = "Parameter 'ordering' has to be of type 'logical' and of length 1.")
})

test_that("gen_find function errors on wrong error.ignore param", {
  expect_error(
    restatis::gen_find(term = "bus", error.ignore = 1, category = "tables"),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})
