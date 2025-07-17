# Test for expected output & API calls------------------------------------------

test_that("We do make POST requests", {

  skip_on_cran()
  skip_on_ci()

  request <- restatis:::gen_api(endpoint = "helloworld/logincheck",
                                database = "genesis")

  expect_equal(request$url,
               "https://www-genesis.destatis.de/genesisWS/rest/2020/helloworld/logincheck")

  expect_equal(request$method,
               "POST")

})
