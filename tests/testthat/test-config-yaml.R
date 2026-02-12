
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
                     "category",
                     "company",
                     "manager") %in% names(y)))

  # minimum properties
  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))

  # if modifier is blank it is populated with value for creator
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))
})

test_that("minimum properties are applied correctly via arguments", {

  x <- suppressWarnings(generate_workbook(as_aftable(demo_df),
                                          creator = "Analysis Function",
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
                     "category",
                     "company",
                     "manager") %in% names(y)))

  # minimum properties
  expect_equal(y["creator"], c("creator" = "Analysis Function"))
  expect_equal(y["title"], c("title" = "example workbook"))
  expect_equal(y["keywords"], c("keywords" = "example, demonstration, config.yaml"))

  # if modifier is blank it is populated with value for creator
  expect_equal(y["modifier"], c("modifier" = "Analysis Function"))

})
