#-------------------------------------------------------------------------------
# Test for expected output & API calls ----
#-------------------------------------------------------------------------------

with_mock_dir("table1", {
  test_that("gen_table returns data.frame", {

    result <- gen_table("61111-0004")

    expect_s3_class(result,
                    class = "data.frame")

    attrs <- attributes(result)

    expect_true("names" %in% names(attrs))
    expect_true("spec" %in% names(attrs))

    })
})

#-------------------------------------------------------------------------------

