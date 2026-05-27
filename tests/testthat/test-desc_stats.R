test_that("desc_table returns a tibble", {
  data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4),
    category = c("x", "y", "x", "y")
  )

  result <- desc_table(data, group_var = "group")

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("variable" %in% names(result))
})
