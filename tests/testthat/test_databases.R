#-------------------------------------------------------------------------------
# Test basic functionality for databases other than genesis, regio and zensus
#-------------------------------------------------------------------------------

test_that("the 'nrw' database performs as expected", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  expect_type(

    res1 <- gen_find(term = "diagnose",
                   database = "nrw",
                   category = "tables"),
    type = "list"

  )

  expect_s3_class(

    res2 <- gen_table(name = "23131-03i",
                      database = "nrw",
                      startyear = 2024),
    class = "data.frame"

  )

  expect_type(

    res3 <- gen_catalogue(code = "",
                          database = "nrw",
                          category = "cubes"),
    type = "list"

  )

  expect_s3_class(

    res4 <- gen_cube(name = "11111GJ001",
                     database = "nrw"),
    class = "data.frame"

  )

})

#-------------------------------------------------------------------------------
