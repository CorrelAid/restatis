# Test for expected output & API calls------------------------------------------

with_mock_dir("terms1", {
  test_that("search terms function returns list", {
    expect_type(
      restatis::running_out_of_terms("forst*", TRUE),
      type = "list")
  })
})

with_mock_dir("terms1", {
  test_that("search terms function returns list", {

      result <- restatis::running_out_of_terms("forst*", TRUE)

      attrs <- attributes(result)

      expect_true("names" %in% names(attrs))
      expect_true("Term" %in% names(attrs))
      expect_true("Language" %in% names(attrs))
      expect_true("Pagelength" %in% names(attrs))
      expect_true("Copyright" %in% names(attrs))

  })
})
