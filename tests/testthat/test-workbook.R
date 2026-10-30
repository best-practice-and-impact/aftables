test_that("workbook object is created", {
  expect_warning(
    wb <- generate_workbook(as_aftable(demo_df)),
    "Some of the recommended workbook properties are missing."
  )

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


test_that("Error if workbook properties arguments are not correct type", {

  expect_error(
    generate_workbook(demo_aftable, author = 1),
    "author must be a character vector of length 1"
  )

  expect_error(
    generate_workbook(demo_aftable, title = c("a", "b")),
    "title must be a character vector of length 1"
  )

  expect_error(
    generate_workbook(demo_aftable, keywords = 2),
    "keywords must be a character vector"
  )

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
  expect_warning(
    wb <- generate_workbook(as_aftable(demo_df)),
    "Some of the recommended workbook properties are missing."
  )

  expect_equal(sum(grepl("HYPERLINK", wb$worksheets[[1]]$sheet_data$cc$f)), 2)
})

test_that("Creating links in a column which doesn't exist causes error", {

  # create a temp config file
  config_file <-
    withr::local_tempfile(
      pattern = "config",
      fileext = ".yaml"
    )

  config_path <- gsub(pattern = "config.*.yaml",
                      x = config_file,
                      "")

  suppressMessages(
    create_config_yaml(path = config_path,
                       open_config = FALSE)
  )

  temp_config <- read_yaml(paste0(config_path, "config.yaml"))

  # demo_df only has 2 columns in the Contents table
  temp_config$aftables$default$workbook_format$config_links <- as.integer(3)

  yaml::write_yaml(x = temp_config,
                   file = paste0(config_path, "config.yaml"))

  expect_error(
    expect_warning(
      generate_workbook(as_aftable(demo_df),
                        config_path = paste0(config_path, "config.yaml")),
      "Your config file contains values identical to the aftables example config."
    ),
    "The column to be turned into internal links does not exist in the Contents table"
  )

})
