# Test for expected output & API calls------------------------------------------

with_mock_dir("modified_data", {
  test_that("modify_data function returns list", {
    expect_type(
      restatis::modified_data(code = "61111", date = "01.01.2022"),
      type = "list")
  })
})
