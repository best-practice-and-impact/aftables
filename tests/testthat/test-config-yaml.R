test_that("no config is applied without config.yaml or function arguments", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df)))

  y <- openxlsx2::wb_get_properties(x)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_true(all(c("creator",
                    "modifier",
                    "datetime_created",
                    "datetime_modified") %in% names(y)))

  # none of the additional properties should be set
  expect_false(all(c("title",
                     "subject",
                     "keywords",
                     "comments",
                     "category") %in% names(y)))

})

test_that("no config if default function arguments are provided", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          author = NULL,
                                          title = NULL,
                                          keywords = NULL,
                                          config_path = "config.yaml",
                                          config_name = NULL))

  y <- openxlsx2::wb_get_properties(x)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_true(all(c("creator",
                    "modifier",
                    "datetime_created",
                    "datetime_modified") %in% names(y)))

  # none of the additional properties should be set
  expect_false(all(c("title",
                     "subject",
                     "keywords",
                     "comments",
                     "category") %in% names(y)))

})

test_that("no config is applied from config.yaml without aftables key", {

  # copy config.yaml file without aftables key
  file.copy(
    from = "./tests/testthat/test_empty_config.yaml",
    to = "./config.yaml",
    overwrite = FALSE,
    copy.mode = FALSE
  )

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df)))

  y <- openxlsx2::wb_get_properties(x)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_true(all(c("creator",
                    "modifier",
                    "datetime_created",
                    "datetime_modified") %in% names(y)))


  # none of the additional properties should be set
  expect_false(all(c("title",
                     "subject",
                     "keywords",
                     "comments",
                     "category") %in% names(y)))

  if (file.exists("config.yaml")) file.remove("config.yaml")

})

test_that("default config.yaml is applied correctly", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          config_path = paste0(testthat::test_path(), "/test_config.yaml"),
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
                                          keywords =  c("example", "demonstration", "config.yaml")))

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
# TODO put these tests back in
# <<<<<<< HEAD
# # error handling
# test_that("config.yaml error handling", {
#   expect_error(generate_workbook(as_aftable(demo_df),
#                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
#                                  config_name = "level_1_errors"),
#                "there were the following errors: 1. workbook_properties must be a list 2. workbook_format must be a list")
#
#   expect_error(generate_workbook(as_aftable(demo_df),
#                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
#                                  config_name = "level_2_errors"),
#                paste("there were the following errors:",
#                      "1. one or more workbook_properties entries are not characters",
#                      "2. keywords must be a list",
#                      "3. base_font_name must be character",
#                      "4. one or more workbook_format entries are not numeric",
#                      "5. decimal_places must be a list",
#                      sep = " "))
#
#   expect_error(generate_workbook(as_aftable(demo_df),
#                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
#                                  config_name = "level_3_errors"),
#                paste("there were the following errors:",
#                      "1. one or more keyword entries are not characters",
#                      "2. decimal_places dataframes entries must be lists",
#                      sep = " "))
#
#   expect_error(generate_workbook(as_aftable(demo_df),
#                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
#                                  config_name = "level_3_errors_2"),
#                paste("there were the following errors:",
#                      "1. one or more dataframes with decimal places entries are not in aftable",
#                      sep = " "))
#
#   expect_error(generate_workbook(as_aftable(demo_df),
#                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
#                                  config_name = "level_4_errors"),
#                paste("there were the following errors:",
#                      "1. one or more columns with specified decimal places is missing from Table_1",
#                      "2. one or more columns with specified decimal places is missing from Table_2",
#                      sep = " "))
# =======
test_that("error when values in config.yaml are blank", {
  expect_error(suppressWarnings(generate_workbook(as_aftable(demo_df),
                                                  config_path = paste0(testthat::test_path(), "/test_blank_config.yaml"),
                                                  config_name = "blank")),
               "Please review the following config.yaml entries")
})

test_that("properties from config.yaml are ignored when properties arguments are set", {
  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          author = "Analysis Function argument",
                                          title = "aftables example workbook argument",
                                          keywords =  c("keywords" = "aftables, example, keywords, argument"),
                                          config_path = paste0(testthat::test_path(), "/test_config.yaml"),
                                          config_name = "mixed-config"))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function argument"))
  expect_equal(y["title"], c("title" = "aftables example workbook argument"))
  expect_equal(y["keywords"], c("keywords" = "aftables, example, keywords, argument"))

})

test_that("error when values in config.yaml are wrong datatype (character/numeric/list)", {
  expect_error(suppressWarnings(generate_workbook(as_aftable(demo_df),
                                                  config_path = paste0(testthat::test_path(), "/test_config.yaml"),
                                                  config_name = "wrong-datatypes")),
               "Please review the following config.yaml entries")
})

test_that("error when aftables cannot find default config requested by user", {
  suppressWarnings(expect_error(generate_workbook(as_aftable(demo_df),
                                                  config_path = paste0(testthat::test_path(), "/test_config_warnings.yaml"),
                                                  config_name = "default"),
                                "The default key doesn't exist in the config file. Please view the documentation for create_config_yaml for an example aftables config file."))

})

test_that("error when both default key and custom key are missing", {
  suppressWarnings(expect_error(generate_workbook(as_aftable(demo_df),
                                                  config_path = paste0(testthat::test_path(), "/test_empty_config.yaml"),
                                                  config_name = "empty"),
                                "The default key and the empty key don't exist in the config file. Please view the documentation for create_config_yaml for an example aftables config file."))

})

test_that("generate_workbook with default arguments finds config.yaml file created with create_config_yaml function", {

  expect_warning(create_config_yaml(open_config = FALSE),
                 "config.yaml copied to working directory. The default options for generate_workbook will use this file.")

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df)))

  y <- openxlsx2::wb_get_properties(x)

  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "aftables example workbook"))
  expect_equal(y["subject"], c("subject" = "aftables example subject"))
  expect_equal(y["keywords"], c("keywords" = "aftables, example, workbook"))
  expect_equal(y["comments"], c("comments" = "aftables example comments"))
  expect_equal(y["category"], c("category" = "aftables example category"))

  if (file.exists("config.yaml")) file.remove("config.yaml")

})
