box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / logic / tables[xts_to_tibble],
  xts[...],
  zoo[index, coredata]
)

# Define test cases
test_that("xts_to_tibble converts xts objects to tibbles", {
  # Create an example xts object
  # Create a sample matrix
  set.seed(123)
  mat <- matrix(rnorm(50), ncol = 5)
  colnames(mat) <- c("col1", "col2", "col3", "col4", "col5")
  rownames(mat) <- format(Sys.Date() - 0:9, "%Y-%m-%d")

  # Convert matrix to xts object
  xts_obj <- xts(mat, order.by = as.Date(rownames(mat), format = "%Y-%m-%d"))

  # Print the xts object
  xts_obj

  # Convert xts object to tibble
  tib_obj <- xts_to_tibble(xts_obj)

  # Check if class of the output is a tibble
  expect_s3_class(tib_obj, "tbl_df")

  # Check if the column names match
  expect_equal(colnames(tib_obj[, -1]), colnames(xts_obj))
})
