#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("terms1", {
  test_that("search terms function returns list", {

    skip_on_cran()
    skip_on_ci()

    result <- restatis::gen_alternative_terms("forst*", TRUE)

    expect_type(result, type = "list")

    attrs <- attributes(result)

    expect_true("Term" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("search term errors on multiple codes", {
  expect_error(
    restatis::gen_alternative_terms(term = c("611*", "711*")),
    regexp = "Parameter 'term' must be a single string.")
})

test_that("search term errors on too long search term", {
  expect_error(
    restatis::gen_alternative_terms(term = "dies das ananas!"),
    regexp = "Parameter 'term' cannot consist of more than 15 characters.")
})

test_that("search term errors on wrong parameter type", {
  expect_error(
    restatis::gen_alternative_terms(term = "Krankenhaus", similarity = 1),
    regexp = "Parameter 'similarity' has to be of type 'logical'.")
})

test_that("search term errors on wrong parameter type", {
  expect_error(
    restatis::gen_alternative_terms(term = 1992, similarity = TRUE),
    regexp = "Parameter 'term' has to be of type 'character'.")
})

