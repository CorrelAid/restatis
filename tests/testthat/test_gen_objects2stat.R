#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

test_that("gen_objects2stat does return a list as result", {

  with_mock_dir("xy_statistic1", {

    skip_on_cran()
    skip_on_ci()

    result <- gen_objects2stat(code = "61111", database = "genesis")

    expect_type(object = result, type = "list")

    attrs <- attributes(result)

    expect_true("Category" %in% names(attrs))
    expect_true("Language" %in% names(attrs))
    expect_true("Pagelength" %in% names(attrs))
    expect_true("Copyright" %in% names(attrs))

  })

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat does return a data.frame for a single category", {

  with_mock_dir("xy_statistic2", {

    skip_on_cran()
    skip_on_ci()

    expect_s3_class(object = gen_objects2stat(code = "61111",
                                              category = "tables",
                                              database = "genesis"),
                    class = "data.frame")

  })

})

#-------------------------------------------------------------------------------
# Test for errors, warnings, messages of parameters ----
#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on multiple codes", {

  expect_error(object = gen_objects2stat(code = c("611*", "711*"),
                                         detailed = TRUE,
                                         category = "tables",
                                         database = "genesis"),
               regexp = "Parameter 'code' must be a single string.")

})

test_that("gen_objects2stat function errors on numeric code param", {

  expect_error(object = gen_objects2stat(code = 12345,
                                         detailed = TRUE,
                                         category = "tables",
                                         database = "genesis"),
               regexp = "Parameter 'code' has to be of type 'character'.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on wrong categories", {

  expect_error(object = gen_objects2stat(code = "611*",
                                         detailed = TRUE,
                                         category = "statistics",
                                         database = "genesis"),
               regexp = "Available categories are 'tables', 'variables', and 'cubes'.")

})

test_that("gen_objects2stat function errors on too many categories", {

  expect_error(object = gen_objects2stat(code = "611*",
                                         detailed = TRUE,
                                         category = c("variables", "statistics", "tables", "cubes"),
                                         dsatabase = "genesis"),
               regexp = "Parameter 'category' has to have a length of 1 to 3.")

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on numeric detailed param", {

  expect_error(object = gen_objects2stat(code = "711*",
                                         detailed = 1,
                                         category = "tables",
                                         database = "genesis"),
               regexp = "Parameter 'detailed' has to be of type 'logical' and of length 1.")

})

test_that("gen_objects2stat function messages on FALSE detailed param", {

  with_mock_dir("xy_statistic3", {

    skip_on_cran()
    skip_on_ci()

    expect_message(object = gen_objects2stat(code = "61111",
                                             detailed = FALSE,
                                             category = "tables",
                                             database = "genesis"),
                   regexp = "Use 'detailed = TRUE' to obtain the complete output.")

  })

})

#-------------------------------------------------------------------------------

test_that("gen_objects2stat function errors on wrong error.ignore param", {

  expect_error(object = gen_objects2stat(code = "711*",
                                         error.ignore = 1),
               regexp = "Parameter 'error.ignore' has to be of type 'logical' and of length 1.")

})
