#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("logincheck1", {

  test_that("gen_logincheck errors when the login failed", {

    skip_on_cran()
    skip_on_ci()

    expect_error(gen_logincheck("genesis"),
                 regexp = "There seems to be an issue with the authentication process")

  })

}, simplify = FALSE)
