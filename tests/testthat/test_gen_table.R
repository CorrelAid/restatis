#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("table1", {

  test_that("gen_table returns data.frame", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_table("61111-0001", startyear = 2019)

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

test_that("gen_table errors on wrong year parameters", {

  expect_error(gen_table("61111-0004", startyear = 1893),
               regexp = "The parameter 'year' has been misspecified")

  expect_error(gen_table("61111-0004", startyear = "1893"),
               regexp = "The parameter 'year' has been misspecified")

})

#-------------------------------------------------------------------------------
