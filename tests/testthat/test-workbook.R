test_that("workbook object is created", {

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df)))

  expect_s4_class(x, class = "Workbook")
  expect_identical(class(x)[1], "Workbook")

})

test_that("aftable is passed", {

  x <- suppressWarnings(as_aftable(demo_df))

  expect_error(generate_workbook("x"))
  expect_error(generate_workbook(1))
  expect_error(generate_workbook(list()))
  expect_error(generate_workbook(data.frame()))

})

test_that(".stop_bad_input works as intended", {

  wb <- openxlsx::createWorkbook()
  aftable <- as_aftable(demo_df)

  expect_error(.stop_bad_input("x", aftable, "cover"))
  expect_error(.stop_bad_input(wb, aftable, 1))

})

test_that("hyperlinks are generated on the cover page", {

  # demo dataset has two hyperlinks on the cover
  y <- suppressWarnings(generate_workbook(as_aftable(demo_df)))
  expect_length(y$worksheets[[1]]$hyperlinks, 2)

})
