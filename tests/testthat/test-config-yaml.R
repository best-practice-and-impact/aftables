
test_that("default config.yaml is applied correctly", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          config_path = testthat::test_path(),
                                          config_file = "test_config.yaml",
                                          config_name = "default"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "aftables example workbook"))
  expect_equal(y["subject"], c("subject" = "aftables example subject"))
  expect_equal(y["keywords"], c("keywords" = "aftables, example, workbook"))
  expect_equal(y["comments"], c("comments" = "aftables example comments"))
  expect_equal(y["category"], c("category" = "aftables example category"))
})

test_that("minimum properties are applied correctly via arguments", {

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          author = "Analysis Function",
                                          title = "example workbook",
                                          keywords =  c("keywords" = "example, demonstration, config.yaml")))

  y <- openxlsx2::wb_get_properties(x)

  # only 6 properties should be set
  expect_equal(names(y), c("creator",
                           "datetime_created",
                           "datetime_modified",
                           "modifier",
                           "title",
                           "keywords"))

  # none of the additional properties should be set
  expect_false(all(c("subject",
                     "comments",
                     "category") %in% names(y)))

  # minimum properties
  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))

  # if modifier is blank it is populated with value for author
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))

})

test_that("error when values in config.yaml are blank", {
  expect_error(suppressWarnings(generate_workbook(as_aftable(demo_df),
                                                  config_path = testthat::test_path(),
                                                  config_file = "test_config.yaml",
                                                  config_name = "blank")),
               "Please review the following config.yaml entries")
})

test_that("properties from arguments are ignored when properties in config.yaml are set", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          author = "Analysis Function argument",
                                          title = "aftables example workbook argument",
                                          keywords =  c("keywords" = "aftables, example, keywords, argument"),
                                          config_path = testthat::test_path(),
                                          config_file = "test_config.yaml",
                                          config_name = "mixed-config"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function config"))
  expect_equal(y["title"], c("title" = "aftables example workbook config"))
  expect_equal(y["keywords"], c("keywords" = "aftables, example, workbook, mixed-config"))

})

test_that("error when values in config.yaml are wrong datatype (character/numeric/list)", {
  expect_error(suppressWarnings(generate_workbook(as_aftable(demo_df),
                                                  config_path = testthat::test_path(),
                                                  config_file = "test_config.yaml",
                                                  config_name = "wrong-datatypes")),
               "Please review the following config.yaml entries")
})

test_that("warning when aftables cannot find configs", {
  suppressWarnings(expect_warning(generate_workbook(as_aftable(demo_df),
                                                    config_path = testthat::test_path(),
                                                    config_file = "test_config_warnings.yaml",
                                                    config_name = "default"),
                                  "default config does not exist in test_config_warnings.yaml. Please check there is a default key in your test_config_warnings.yaml."))

  suppressWarnings(expect_warning(generate_workbook(as_aftable(demo_df),
                                                    config_path = testthat::test_path(),
                                                    config_file = "test_config_warnings.yaml",
                                                    config_name = "user-config"),
                                  "user set config does not exist in test_config_warnings.yaml. Please check there is a user-config key in your test_config_warnings.yaml."))

})

test_that("generate_workbook finds default config.yaml file created with create_config_yaml function", {

  expect_warning(create_config_yaml(),
                 "config.yaml copied to working directory. The default options for generate_workbook will use this file.")

  x <- generate_workbook(as_aftable(demo_df))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "aftables example workbook"))
  expect_equal(y["subject"], c("subject" = "aftables example subject"))
  expect_equal(y["keywords"], c("keywords" = "aftables, example, workbook"))
  expect_equal(y["comments"], c("comments" = "aftables example comments"))
  expect_equal(y["category"], c("category" = "aftables example category"))

  if (file.exists("./config.yaml")) file.remove("./config.yaml")

})
