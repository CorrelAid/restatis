#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("logincheck1_fake", {

  test_that("gen_logincheck errors when the login failed (fake response)", {

    # The mockfile needs to be altered with respect to the body/content
    # It needs to include the string "Ein Fehler ist aufgetreten"
    # So that the below error message is displayed

    skip_on_cran()
    skip_on_ci()

    expect_error(object = gen_logincheck("genesis"),
                 regexp = "There seems to be an issue with the authentication process")

  })

}, simplify = FALSE)
