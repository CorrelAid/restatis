# Test for expected output & API calls------------------------------------------

with_mock_dir("variables1", {
  test_that("get_variables_from_statistic function returns list", {
    expect_type(
      restatis::get_variables_from_statistic(code = "61111",
                                             detailed = TRUE,
                                             sortcriterion = "code"),
      type = "list")
  })
})

#-------------------------------------------------------------------------------

# with_mock_dir("variables4", {
#   test_that("get_values_from_variables_from_statistic function returns list", {
#     expect_type(
#       restatis::get_values_from_variables_from_statistic(code = "21111",
#                                                          detailed = TRUE),
#       type = "list")
#   })
# })

# search_variables

