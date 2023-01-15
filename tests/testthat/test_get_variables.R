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

with_mock_dir("variables2", {
  test_that("get_values_from_variables function errors if there is no result", {
    expect_error(
      restatis::get_values_from_variables(code = "61111",
                                          detailed = TRUE,
                                          sortcriterion = "code"),
      regexp = "Es gibt keine Objekte zum angegebenen Selektionskriterium")
  })
})

#-------------------------------------------------------------------------------

with_mock_dir("variables3", {
  test_that("get_values_from_variables function returns list", {
    expect_type(
      restatis::get_values_from_variables(code = "DLAND",
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

