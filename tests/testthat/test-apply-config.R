test_that("no config is applied without config.yaml or function arguments", {
  wb <- generate_workbook(
    as_aftable(demo_df),
    config_path = NULL
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_equal(
    names(wb_properties),
    c("creator", "datetime_created", "datetime_modified", "modifier")
  )

})

test_that("default config.yaml is applied correctly", {

  expect_warning(
    wb <- generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("test_config.yaml")
    ),
    "Your config file contains values identical to the aftables example config. Please check your config file."
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  expect_equal(wb_properties["creator"], c("creator" = "Analysis Function"))
  expect_equal(wb_properties["modifier"], c("modifier" = "Analysis Function"))
  expect_equal(wb_properties["title"], c("title" = "aftables example workbook"))
  expect_equal(wb_properties["subject"], c("subject" = "aftables example subject"))
  expect_equal(wb_properties["keywords"], c("keywords" = "aftables, example, workbook"))
  expect_equal(wb_properties["comments"], c("comments" = "aftables example comments"))
  expect_equal(wb_properties["category"], c("category" = "aftables example category"))
})

test_that("minimum properties are applied correctly via arguments", {
  expect_warning(
    wb <- generate_workbook(
      as_aftable(demo_df),
      author = "Analysis Function",
      title = "example workbook",
      keywords =  c("example", "demonstration", "config.yaml"),
      config_path = NULL
    ),
    "Your config file contains values identical to the aftables example config. Please check your config file."
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # only 6 properties should be set
  expect_equal(names(wb_properties),
               c("creator",
                 "datetime_created",
                 "datetime_modified",
                 "modifier",
                 "title",
                 "keywords"))

  # minimum properties
  expect_equal(wb_properties["creator"],
               c("creator" = "Analysis Function"))
  expect_equal(wb_properties["title"],
               c("title" = "example workbook"))
  expect_equal(wb_properties["keywords"],
               c("keywords" = "example, demonstration, config.yaml"))

  # if modifier is blank it is populated with value for author
  expect_equal(wb_properties["modifier"],
               c("modifier" = "Analysis Function"))

})

test_that("properties from config.yaml are ignored when properties arguments are set", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    author = "Analysis Function argument",
    title = "aftables example workbook argument",
    keywords =  c("keywords" = "aftables, example, keywords, argument"),
    config_path = testthat::test_path("test_config.yaml"),
    config_name = "mixed-config"
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # Properties from function arguments
  expect_equal(wb_properties["creator"],
               c("creator" = "Analysis Function argument"))
  expect_equal(wb_properties["title"],
               c("title" = "aftables example workbook argument"))
  expect_equal(wb_properties["keywords"],
               c("keywords" = "aftables, example, keywords, argument"))

  # Properties from default config
  expect_equal(wb_properties["subject"],
               c("subject" = "aftables example subject config"))

})
