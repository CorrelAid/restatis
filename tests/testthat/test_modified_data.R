# Test for expected output & API calls------------------------------------------

with_mock_dir("modified_data", {
  test_that("modify_data function returns list", {
    expect_type(
      restatis::modified_data(code = "61111", date = "01.01.2022"),
      type = "list")
  })
})

# Test for function parameter errors -------------------------------------------

test_that("modify_data errors on misspecified dates", {
  expect_error(
    restatis::modified_data(code = "61111", date = "1.1.2022"),
    regexp = "it has to be of length 1 and format DD.MM.YYYY")
})

test_that("modify_data errors on misspecified dates", {
  expect_error(
    restatis::modified_data(code = "61111", date = 23456),
    regexp = "'date', it has to be of type 'character'.")
})
