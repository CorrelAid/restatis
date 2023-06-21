# Test for expected output & API calls------------------------------------------

without_internet({
  test_that("We do make GET requests", {

    skip_on_cran()
    skip_on_ci()

    expect_GET(
      restatis:::gen_api(endpoint = "helloworld/whoami"),
      "https://www-genesis.destatis.de/genesisWS/rest/2020/helloworld/whoami"
    )
  })
})
