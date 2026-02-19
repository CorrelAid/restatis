#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

test_that("gen_table returns data.frame", {

  with_mock_dir("table1", {

    skip_on_cran()

    result <- gen_table(name = "61111-0001",
                        startyear = 2023,
                        database = "genesis")

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

test_that("gen_table errors on wrong year parameters", {

  expect_error(object = gen_table(name = "61111-0004",
                                  startyear = 1893,
                                  database = "genesis"),
               regexp = "The parameter 'year' has been misspecified")

  expect_error(object = gen_table(name = "61111-0004",
                                  startyear = "1893",
                                  database = "genesis"),
               regexp = "The parameter 'year' has been misspecified")

})

#-------------------------------------------------------------------------------
