# Test for expected output & API calls------------------------------------------

without_internet({

  test_that("We do make POST requests", {

    skip_on_cran()
    skip_on_ci()

    expect_POST(restatis:::gen_api(endpoint = "helloworld/whoami",
                                   database = "genesis"),
               "https://www-genesis.destatis.de/genesisWS/rest/2020/helloworld/whoami")

  })

})
