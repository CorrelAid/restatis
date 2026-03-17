test_that("proxy settings are passed to req_proxy", {
  local_mocked_bindings(
    req_proxy = function(req, url, port, username, password, auth, ...) {
      expect_equal(url, "localhost")
      expect_equal(port, 8888)
      expect_equal(username, "user")
      expect_equal(password, "pass")
      expect_equal(auth, "basic")
      req
    },
    .package = "httr2"
  )

  req <- httr2::request("https://example.com") |>
    req_proxy_if_set(list(
      url      = "localhost",
      port     = 8888,
      username = "user",
      password = "pass",
      auth     = "basic"
    ))
})

test_that("NULL proxy is a no-op", {
  req <- httr2::request("https://example.com")
  result <- req_proxy_if_set(req, proxy = NULL)
  expect_identical(req, result)
})

test_that("global option is picked up", {
  withr::with_options(
    list(restatis.proxy = list(
      url      = "localhost",
      port     = 8888,
      username = NULL,
      password = NULL,
      auth     = "basic"
    )),
    {
      local_mocked_bindings(
        req_proxy = function(req, ...) req,
        .package = "httr2"
      )
      req <- httr2::request("https://example.com") |>
        req_proxy_if_set()
      expect_true(TRUE)
    }
  )
})

test_that("gen_set_proxy sets option correctly", {
  withr::with_options(list(restatis.proxy = NULL), {
    gen_set_proxy("localhost", port = 8888, username = "user",
                  password = "pass", auth = "ntlm")
    proxy <- getOption("restatis.proxy")
    expect_equal(proxy$url,      "localhost")
    expect_equal(proxy$port,     8888)
    expect_equal(proxy$username, "user")
    expect_equal(proxy$password, "pass")
    expect_equal(proxy$auth,     "ntlm")
  })
})

test_that("gen_set_proxy defaults are correct", {
  withr::with_options(list(restatis.proxy = NULL), {
    gen_set_proxy("localhost")
    proxy <- getOption("restatis.proxy")
    expect_null(proxy$port)
    expect_null(proxy$username)
    expect_null(proxy$password)
    expect_equal(proxy$auth, "basic")
  })
})
