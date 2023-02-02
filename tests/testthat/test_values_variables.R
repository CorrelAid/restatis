# # Test for expected output & API calls------------------------------------------
#
# with_mock_dir("values1", {
#   test_that("get_values_from_variables function errors if there is no result", {
#     expect_error(
#       restatis::get_values_from_variables(code = "61111",
#                                           detailed = TRUE,
#                                           sortcriterion = "code"),
#       regexp = "Es gibt keine Objekte zum angegebenen Selektionskriterium")
#   })
# })
#
# #-------------------------------------------------------------------------------
#
# with_mock_dir("variables3", {
#   test_that("get_values_from_variables function returns list", {
#     expect_type(
#       restatis::get_values_from_variables(code = "DLAND",
#                                           detailed = TRUE,
#                                           sortcriterion = "code"),
#       type = "list")
#   })
# })
