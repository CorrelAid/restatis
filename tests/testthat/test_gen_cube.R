#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

test_that("gen_cube returns data.frame", {

  with_mock_dir("cube1", {

    skip_on_cran()

    result <- gen_cube(name = "47414BJ002",
                       database = "genesis",
                       startyear = 2024)

    expect_s3_class(object = result,
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

  expect_error(object = gen_cube(name = "47414BJ002",
                                 database = "genesis",
                                 startyear = 1893),
               regexp = "The parameter 'year' has been misspecified")

  expect_error(object = gen_cube(name = "47414BJ002",
                                 database = "genesis",
                                 startyear = "1893"),
               regexp = "The parameter 'year' has been misspecified")

})

#-------------------------------------------------------------------------------
