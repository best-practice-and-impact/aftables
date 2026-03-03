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

test_that("no config if default function arguments are provided and config.yaml doesn't exist", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    author = NULL,
    title = NULL,
    keywords = NULL,
    config_path = "config.yaml",
    config_name = NULL
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_equal(
    names(wb_properties),
    c("creator", "datetime_created", "datetime_modified", "modifier")
  )

})

test_that("error if invalid config file", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("test_missing_aftable_config.yaml"),
      config_name = NULL
    ),
    "does not contain an aftables key"
  )

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("test_missing_default_config.yaml"),
      config_name = NULL
    ),
    "does not contain a default aftables configuration and a custom key is not being used"
  )

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("test_missing_default_config.yaml"),
      config_name = NULL
    ),
    "does not contain a default aftables configuration and a custom key is not being used"
  )

})

test_that("default config.yaml is applied correctly", {

  expect_warning(
    wb <- generate_workbook(as_aftable(demo_df),
                            config_path = testthat::test_path("test_config.yaml")),
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
    wb <- generate_workbook(as_aftable(demo_df),
                            author = "Analysis Function",
                            title = "example workbook",
                            keywords =  c("example", "demonstration", "config.yaml")),
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

  wb <- generate_workbook(as_aftable(demo_df),
                          author = "Analysis Function argument",
                          title = "aftables example workbook argument",
                          keywords =  c("keywords" = "aftables, example, keywords, argument"),
                          config_path = testthat::test_path("test_config.yaml"),
                          config_name = "mixed-config")

  wb_properties <- openxlsx2::wb_get_properties(wb)

  expect_equal(wb_properties["creator"],
               c("creator" = "Analysis Function argument"))
  expect_equal(wb_properties["title"],
               c("title" = "aftables example workbook argument"))
  expect_equal(wb_properties["keywords"],
               c("keywords" = "aftables, example, keywords, argument"))

})

test_that("error when values in config.yaml are wrong datatype (character/numeric/list)", {

  expect_error(
    expect_warning(
      generate_workbook(as_aftable(demo_df),
                        config_path = testthat::test_path("test_config.yaml"),
                        config_name = "wrong-datatypes"),
      "The config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Please review the following invalid config entries"
  )

})

test_that("error when aftables cannot find default config requested by user", {
  expect_error(
    expect_warning(
      generate_workbook(as_aftable(demo_df),
                        config_path = testthat::test_path("test_config_warnings.yaml"),
                        config_name = "default"),
      "The config file contains values identical to the aftables example config. Please check your config file."
    ),
    "does not contain key `default`"
  )

})

test_that("error when entries apart from keywords have more than 1 value", {
  expect_error(
    expect_warning(
      generate_workbook(as_aftable(demo_df),
                        config_path = testthat::test_path("test_config.yaml"),
                        config_name = "wrong-lengths"),
      "Your config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Config entries must contain only one value apart from keywords. Please check your config file."
  )
})

test_that("error when config entries have invalid names or in wrong place", {
  expect_warning(
    expect_warning(
      generate_workbook(as_aftable(demo_df),
                        config_path = testthat::test_path("test_wrong_config.yaml")),
      "Your config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Some entries in your config file could not be processed."
  )
})
