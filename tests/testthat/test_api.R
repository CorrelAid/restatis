# Test for expected output & API calls------------------------------------------

without_internet({
  test_that("We do make GET requests", {
    expect_GET(
      restatis:::gen_api(endpoint = "helloworld/whoami"),
      "https://www-genesis.destatis.de/genesisWS/rest/2020/helloworld/whoami"
    )
  })
})

