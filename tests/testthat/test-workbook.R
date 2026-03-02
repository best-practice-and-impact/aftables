test_that("workbook object is created", {
  wb <- generate_workbook(as_aftable(demo_df))

  expect_s3_class(wb, class = c("wbWorkbook", "R6"))
  expect_identical(class(wb)[1], "wbWorkbook")
})

test_that("aftable is passed", {
  wb <- suppressWarnings(as_aftable(demo_df))

  expect_error(generate_workbook("wb"))
  expect_error(generate_workbook(1))
  expect_error(generate_workbook(list()))
  expect_error(generate_workbook(data.frame()))
})

test_that(".stop_bad_input works as intended", {
  wb <- openxlsx2::wb_workbook()

  test_aftable <- as_aftable(demo_df)

  expect_error(.stop_bad_input("wb", test_aftable, "cover"),
               "'wb' must be an openxlsx2 wbWorkbook-class object.")
  expect_error(.stop_bad_input(wb, test_aftable, 1),
               "'table_name' must be a string of length 1")
})

test_that("hyperlinks are generated on the cover page", {
  # demo dataset has two hyperlinks on the cover
  wb <- generate_workbook(as_aftable(demo_df))

  expect_equal(sum(grepl("HYPERLINK", wb$worksheets[[1]]$sheet_data$cc$f)), 2)
})
