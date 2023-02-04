#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("catalogue1", {
  test_that("catalogue function returns list for tables", {

      result <- restatis::catalogue(code = "611*",
                                    detailed = TRUE,
                                    category = "tables",
                                    error.ignore = FALSE)

      expect_type(result, type = "list")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("catalogue2", {
  test_that("catalogue function returns lists for statistics", {
    expect_type(
      restatis::catalogue(code = "41141",
                          detailed = FALSE,
                          category = "statistics",
                          error.ignore = FALSE),
      type = "list")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("catalogue3", {
  test_that("catalogue function returns error if there are no results", {
    expect_error(
      restatis::catalogue(code = "41141",
                          detailed = FALSE,
                          category = "cubes",
                          error.ignore = FALSE),
      regexp = "No object found for your request")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("catalogue4", {
  test_that("catalogue function returns list of length 3 if all categories are selected", {

    res <- restatis::catalogue(code = "611*",
                               detailed = FALSE,
                               error.ignore = TRUE)

    expect_equal(length(res), 3)

  })
})

with_mock_dir("catalogue5", {
  test_that("catalogue function messages on TRUE error.ignore param", {
    expect_message(
      restatis::catalogue(code = "711*", detailed = TRUE, category = "tables", error.ignore = TRUE),
      regexp = "Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")
  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("catalogue function errors on multiple codes", {
  expect_error(
    restatis::catalogue(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")
})

test_that("catalogue function errors on numeric code param", {
  expect_error(
    restatis::catalogue(code = 12345, detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' has to be of type 'character' or NULL.")
})

#-------------------------------------------------------------------------------

test_that("catalogue function errors on wrong categories", {
  expect_error(
    restatis::catalogue(code = "611*", detailed = TRUE, category = "variables"),
    regexp = "Available categories are tables, statistics, and cubes.")
})

test_that("catalogue function errors on too many categories", {
  expect_error(
    restatis::catalogue(code = "611*", detailed = TRUE,
                        category = c("variables", "statistics", "tables", "cubes")),
    regexp = "Parameter 'category' has to have a length of 1 to 3.")
})

#-------------------------------------------------------------------------------

test_that("catalogue function errors on numeric detailed param", {
  expect_error(
    restatis::catalogue(code = "711*", detailed = 1, category = "tables"),
    regexp = "Paramter 'detailed' has to be of type 'logical' and of length 1.")
})

test_that("catalogue function messages on FALSE detailed param", {
  expect_message(
    restatis::catalogue(code = "711*", detailed = FALSE, category = "tables"),
    regexp = "Use 'detailed = TRUE' to obtain the complete output.")
})

#-------------------------------------------------------------------------------

test_that("catalogue function errors on numeric sortcriterion param", {
  expect_error(
    restatis::catalogue(code = "711*", detailed = TRUE, category = "tables", sortcriterion = c(1, 2)),
    regexp = "Parameter 'sortcriterion' has to be of type 'character'")
})

#-------------------------------------------------------------------------------

test_that("catalogue function errors on wrong error.ignore param", {
  expect_error(
    restatis::catalogue(code = "711*", detailed = TRUE, category = "tables", error.ignore = 1),
    regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")
})

