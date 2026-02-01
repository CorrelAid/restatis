# Test gen_auth_path --------------------------------------------------------

test_that("gen_auth_path returns correct file path", {

  skip_on_ci()
  skip_on_cran()

  path <- gen_auth_path()
  expect_true(grepl("restatis", path))
  expect_true(dir.exists(dirname(path)) || !file.exists(path))

})

test_that("gen_auth_path handles additional arguments", {

  skip_on_ci()
  skip_on_cran()

  path <- gen_auth_path("test.rds")
  expect_true(grepl("test.rds$", path))

})

# Test gen_auth_save --------------------------------------------------------

test_that("gen_auth_save validates database parameter", {

  expect_error(gen_auth_save(database = "invalid"),
               "database")

})

# Test gen_auth_get ---------------------------------------------------------
test_that("gen_auth_get returns credentials when they exist", {

  skip_on_ci()
  skip_on_cran()

  # This requires saved credentials to test properly
  skip_if_not(file.exists(gen_auth_path("auth_genesis.rds")),
              "No genesis credentials saved!")

  creds <- gen_auth_get(database = "genesis")
  expect_type(creds, "list")
  expect_true(all(c("username", "password") %in% names(creds) |
                    "token" %in% names(creds)))

})

test_that("gen_auth_get validates database parameter", {

  expect_error(gen_auth_get(database = "invalid"),
               "database")

})

test_that("gen_auth_get accepts all valid databases", {

  skip_on_ci()
  skip_on_cran()

  valid_dbs <- c("genesis", "zensus", "regio")

  for (db in valid_dbs) {

    skip_if_not(file.exists(gen_auth_path(paste0("auth_", db, ".rds"))),
                paste("No", db, "credentials!"))

    expect_silent(gen_auth_get(database = db))

  }

})
