#-------------------------------------------------------------------------------
# Test basic functionality for databases other than genesis, regio and zensus
#-------------------------------------------------------------------------------

# As of Feb. 2026 these are: "nrw", "bayern", "st", and "bildung"

test_that("the 'nrw' database performs as expected", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  expect_type(object = res1 <- gen_find(term = "diagnose",
                                        database = "nrw",
                                        category = "tables"),
              type = "list")

  expect_s3_class(object = res2 <- gen_table(name = "23131-03i",
                                             database = "nrw",
                                             startyear = 2024),
                  class = "data.frame")

  expect_type(object = res3 <- gen_catalogue(code = "",
                                             database = "nrw",
                                             category = "cubes"),
              type = "list")

  expect_s3_class(object = res4 <- gen_cube(name = "11111GJ001",
                                            database = "nrw"),
                  class = "data.frame")

})

#-------------------------------------------------------------------------------

# This works, though it throws a warning due to the parsing errors
# that are caused by broken files distributed by 'bayern'
# Reactivate when the parsing errors are resolved

# test_that("the 'bayern' database performs as expected", {
#
#   skip_on_ci()
#   skip_on_cran()
#   skip_if_offline()
#
#   expect_type(object = res1 <- gen_find(term = "bus*",
#                                         database = "bayern",
#                                         category = "tables"),
#               type = "list")
#
#   expect_s3_class(object = res2 <- gen_table(name = "61111-301z",
#                                              database = "bayern",
#                                              startyear = 2021,
#                                              endyear = 2022),
#                   class = "data.frame")
#
#   expect_type(object = res3 <- gen_catalogue(code = "23*",
#                                              database = "bayern"),
#               type = "list")
#
# })

#-------------------------------------------------------------------------------

test_that("the 'st' database performs as expected", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  expect_type(object = res1 <- gen_find(term = "diagnose",
                                        database = "st",
                                        category = "tables"),
              type = "list")

  expect_s3_class(object = res2 <- gen_table(name = "23211-0001",
                                             database = "st",
                                             startyear = 2021,
                                             endyear = 2022),
                  class = "data.frame")

  expect_type(object = res3 <- gen_catalogue(code = "23*",
                                             database = "st"),
              type = "list")

})

#-------------------------------------------------------------------------------

test_that("the 'bildung' database performs as expected", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  expect_type(object = res1 <- gen_find(term = "realschule",
                                        database = "bildung",
                                        category = "tables"),
              type = "list")

  expect_s3_class(object = res2 <- gen_table(name = "BW-D07.1i",
                                             database = "bildung",
                                             startyear = 2024),
                  class = "data.frame")

  expect_type(object = res3 <- gen_catalogue(code = "",
                                             database = "bildung",
                                             category = "cubes"),
              type = "list")

  expect_s3_class(object = res4 <- gen_cube(name = "02--A014C",
                                            startyear = 2023,
                                            database = "bildung"),
                  class = "data.frame")

})
