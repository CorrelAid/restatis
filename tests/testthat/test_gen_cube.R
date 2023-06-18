#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("cube1", {

  test_that("gen_cube returns data.frame", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_cube("47414BJ002")

    expect_s3_class(result,
                    class = "data.frame")

    attrs <- attributes(result)

    expect_true("names" %in% names(attrs))
    expect_true("spec" %in% names(attrs))

  })
})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_cube errors on wrong year parameters", {

  expect_error(gen_cube("47414BJ002", startyear = 1893),
               regexp = "The parameter 'year' has been misspecified")

  expect_error(gen_cube("47414BJ002", startyear = "1893"),
               regexp = "The parameter 'year' has been misspecified")

})

#-------------------------------------------------------------------------------
