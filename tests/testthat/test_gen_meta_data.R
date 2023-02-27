#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("meta1", {
  test_that("gen_meta_data returns list", {

    result <- restatis::gen_meta_data(code = "11111-0001",
                                      category = "Table")

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Code" %in% names(attrs))
    expect_true("Method" %in% names(attrs))
    expect_true("Updated" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("meta2", {
  test_that("gen_meta_data errors if there is an error code", {
    expect_error(
      restatis::gen_meta_data(code = "1*",
                              category = "Cube"),
      regexp = "test error message")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("meta3", {
  test_that("gen_meta_data gives message if error.ignore = TRUE", {
    expect_message(
      restatis::gen_meta_data(code = "11111-0001",
                              category = "Table",
                              error.ignore = TRUE),
      regexp = "Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_meta_data function errors on numeric code param", {
  expect_error(
    restatis::gen_meta_data(code = 12345, category = "Table"),
    regexp = "Parameter 'code' has to be of type 'character'.")
})

test_that("gen_meta_data function errors on multiple categories", {
  expect_error(
    restatis::gen_meta_data(code = "12345", category = c("Table", "Cube")),
    regexp = "Parameter 'category' must have a length of 1. Please specify the category.")
})

test_that("gen_meta_data function errors on wrong category", {
  expect_error(
    restatis::gen_meta_data(code = "11111", category = "table"),
    regexp = "Available categories are Cube, Table, Statistic, Variable, and Value.")
})

test_that("gen_meta_data function errors on wrong error.ignore param", {
  expect_error(
    restatis::gen_meta_data(code = "11111", error.ignore = 1, category = "Table"),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})


