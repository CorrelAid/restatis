# Test for expected output & API calls------------------------------------------

with_mock_dir("catalogue1", {
  test_that("catalogue function returns lists for tables using * notation", {
    expect_type(
      restatis::catalogue(code = "611*", detailed = TRUE, category = "tables"),
      type = "list")
  })
})

with_mock_dir("catalogue2", {
  test_that("catalogue function returns lists for statistics", {
    expect_type(
      restatis::catalogue(code = "41141", detailed = FALSE, category = "statistics"),
      type = "list")
  })
})

with_mock_dir("catalogue3", {
  test_that("catalogue function returns error if there are no results", {
    expect_warning(
      restatis::catalogue(code = "41141", detailed = FALSE, category = "cubes"),
      regexp = "Es gibt keine Objekte")
  })
})

# Test for errors, warnings, messages ------------------------------------------

test_that("catalogue function errors on multiple codes", {
  expect_error(
    restatis::catalogue(code = c("611*", "711*"), detailed = TRUE, category = "tables"),
    regexp = "Parameter 'code' must be a single string.")
})

