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

test_that("default config.yaml is applied correctly", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          config_path = paste0(testthat::test_path(), "/test_config.yaml"),
                                          config_name = "default"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["subject"], c("subject" = "example subject"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))
  expect_equal(y["comments"], c("comments" = "example"))
  expect_equal(y["category"], c("category" = "example"))
  expect_equal(y["company"], c("company" = "Analysis Function"))
  expect_equal(y["manager"], c("manager" = "Analysis Function manager"))
})

test_that("minimum config.yaml is applied correctly", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          config_path = paste0(testthat::test_path(), "/test_config.yaml"),
                                          config_name = "minimum"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))
})

test_that("minimum properties are applied correctly via arguments", {

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          creator = "Analysis Function",
                                          title = "example workbook",
                                          keywords =  c("keywords" = "example, demonstration, config.yaml")))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))
})

test_that("empty properties are not added to workbook", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          config_path = paste0(testthat::test_path(), "/test_config.yaml"),
                                          config_name = "empty"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))
})
