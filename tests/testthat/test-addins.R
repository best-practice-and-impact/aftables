test_that("string_create_aftable skeleton is okay", {
  expect_snapshot_output(string_create_aftable())
})

test_that("string_helper_functions skeleton is okay", {
  expect_snapshot_output(string_helper_functions())
})

test_that("string_tables_tibble skeleton is okay", {
  expect_snapshot_output(string_tables())
})
