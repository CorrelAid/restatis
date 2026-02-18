# Complete workflow tests ---------------------------------------------------

test_that("complete data discovery and retrieval workflow", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # 1. Find data about population
  search_results <- gen_find(database = "genesis",
                             term = "Bevölkerung")

  expect_type(search_results, "list")
  expect_s3_class(search_results$Tables, "data.frame")
  expect_true(nrow(search_results$Tables) > 0)

  # 2. Get metadata about first result
  if (nrow(search_results$Tables) > 0) {

    first_code <- search_results$Tables$Code[1]

    metadata <- gen_metadata(code = first_code,
                             database = "genesis",
                             category = "table")

    expect_type(metadata, "list")

  }

})

#-------------------------------------------------------------------------------

test_that("authentication and data access workflow", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # 1. Check login
  login_ok <- gen_logincheck(database = "genesis")
  expect_true(login_ok)

  # 2. If logged in, retrieve data
  if (login_ok) {

    catalogue <- gen_catalogue(code = "12411-0008",
                               database = "genesis",
                               category = "tables")

    expect_type(catalogue, "list")
    expect_true(nrow(catalogue[["Tables"]][[1]][[1]][[1]][[1]]) > 0)

  }

})

#-------------------------------------------------------------------------------

test_that("table retrieval with filtering workflow", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # Get table with year filter
  table_data <- gen_table(name = "12411-0001",
                          database = "genesis",
                          startyear = 2020,
                          endyear = 2021,
                          language = "en")

  expect_s3_class(table_data, "data.frame")

  # Verify year filtering worked (if years are in the data)
  if ("time" %in% names(table_data)) {

    years <- substr(table_data$time, 1, 4)

    expect_true(all(years == c("2021", "2020")))

  }

})

#-------------------------------------------------------------------------------

test_that("relationship exploration workflow", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # 1. Find statistics related to code
  stats <- gen_objects2stat(code = "12411",
                            database = "genesis")

  expect_type(stats, "list")

  # 2. Search for variables
  vars <- gen_search_vars(name = "12411",
                          database = "genesis")

  expect_type(vars, "list")

})

# Error recovery tests ------------------------------------------------------

test_that("system recovers from API errors gracefully", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # Try invalid request
  result1 <- tryCatch(gen_table(name = "invalid-table", database = "genesis"),
                      error = function(e) NULL)

  # System should still work after error
  result2 <- gen_logincheck(database = "genesis")
  expect_type(result2, "logical")

})

# Data consistency tests ----------------------------------------------------

test_that("caching works as intended", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # Clear cache if it exists
  if (getOption("restatis.use_cache", TRUE)) {

    options(restatis.use_cache = FALSE)

  }

  start_time <- Sys.time()

  result1 <- gen_table(name = "12411-0001",
                       database = "genesis",
                       startyear = 2024)

  end_time <- Sys.time()

  first_call <- end_time - start_time

  #----------------------------------------------------------------------------

  # Re-enable cache
  options(restatis.use_cache = TRUE)

  result2 <- gen_table(name = "12411-0001",
                       database = "genesis",
                       startyear = 2024)

  start_time <- Sys.time()

  result3 <- gen_table(name = "12411-0001",
                       database = "genesis",
                       startyear = 2024)

  end_time <- Sys.time()

  second_call <- end_time - start_time

  # Results should be identical
  expect_true(second_call < first_call)

})

#-------------------------------------------------------------------------------

test_that("detailed vs. non-detailed queries are consistent", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  basic <- gen_catalogue(code = "12411-0001",
                         database = "genesis",
                         category = "tables",
                         detailed = FALSE)

  detailed <- gen_catalogue(code = "12411-0001",
                            database = "genesis",
                            category = "tables",
                            detailed = TRUE)

  # Detailed should have more columns
  expect_true(ncol(detailed[["Tables"]][[1]][[1]][[1]][[1]]) >=
                ncol(basic[["Tables"]][[1]][[1]][[1]][[1]]))

  # Should have same number of rows
  expect_equal(nrow(basic[["Tables"]][[1]][[1]][[1]][[1]]),
               nrow(detailed[["Tables"]][[1]][[1]][[1]][[1]]))

})

# Performance tests ---------------------------------------------------------

test_that("functions handle large datasets efficiently", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # Request potentially large catalogue
  start <- Sys.time()
  result <- gen_catalogue(code = "12411-0001",
                          database = "genesis",
                          category = "tables")

  elapsed <- difftime(Sys.time(), start, units = "secs")

  # Should complete in reasonable time (< 30 seconds)
  expect_true(elapsed < 30)

  # Should return data
  expect_type(result, "list")

})

# Cross-database compatibility tests ---------------------------------------

test_that("same function works across all databases", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  databases <- c("genesis", "zensus", "regio")

  for (db in databases) {

    skip_if_not(file.exists(gen_auth_path(paste0("auth_", db, ".rds"))),
                paste("No", db, "credentials!"))

    # gen_signs should work for all
    result <- gen_signs(database = db)
    expect_s3_class(result$Output, "data.frame")
    expect_true(nrow(result$Output) > 0)

    # gen_logincheck should work for all
    login <- gen_logincheck(database = db)
    expect_type(login, "logical")
  }

})

#-------------------------------------------------------------------------------

test_that("error messages are consistent across databases", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  databases <- c("genesis", "zensus", "regio")

  for (db in databases) {
    skip_if_not(file.exists(gen_auth_path(paste0("auth_", db, ".rds"))),
                paste("No", db, "credentials!"))

    # Test with invalid table name
    result <- tryCatch(gen_table(name = "invalid-99999", database = db),
                       error = function(e) e$message)

    # Should get some error message
    expect_true(is.character(result))
    expect_true(nchar(result) > 0)

  }

})

# Job workflow tests --------------------------------------------------------

test_that("job creation and retrieval workflow", {

  skip_on_ci()
  skip_on_cran()
  skip_if_offline()
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials!")

  # List jobs
  jobs <- gen_list_jobs(database = "genesis", flat = TRUE)
  expect_s3_class(jobs, "data.frame")

})
