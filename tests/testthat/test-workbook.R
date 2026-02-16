test_that("workbook object is created", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df)))

  expect_s3_class(x, class = c("wbWorkbook", "R6"))
  expect_identical(class(x)[1], "wbWorkbook")
})

test_that("aftable is passed", {
  x <- suppressWarnings(as_aftable(demo_df))

  expect_error(generate_workbook("x"))
  expect_error(generate_workbook(1))
  expect_error(generate_workbook(list()))
  expect_error(generate_workbook(data.frame()))
})

test_that(".stop_bad_input works as intended", {
  wb <- openxlsx2::wb_workbook()
  aftable <- as_aftable(demo_df)

  expect_error(.stop_bad_input("x", aftable, "cover"))
  expect_error(.stop_bad_input(wb, aftable, 1))
})

test_that("hyperlinks are generated on the cover page", {
  # demo dataset has two hyperlinks on the cover
  y <- suppressWarnings(generate_workbook(as_aftable(demo_df)))
  expect_equal(sum(grepl("HYPERLINK", y$worksheets[[1]]$sheet_data$cc$f)), 2)
})
