# Test .process_config ---------------------------------------------------------

test_that("error if config file doesn't exist", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("configs/DOES_NOT_EXIST.yaml"),
      config_name = NULL
    ),
    "DOES_NOT_EXIST.yaml does not exist"
  )

})

test_that("error if config doesn't have aftables key", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("configs/test_missing_aftable_config.yaml"),
      config_name = NULL
    ),
    "does not contain an aftables key"
  )

})

test_that("error if default is missing and not using custom key", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("configs/test_missing_default_config.yaml"),
      config_name = NULL
    ),
    "does not contain a default aftables configuration and a custom key is not being used"
  )

})

test_that("error if default config not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_default.yaml")
    ),
    "Default configuration key must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_default2.yaml")
    ),
    "Default configuration key must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

})

test_that("error if default workbook properties is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_default3.yaml")
    ),
    "Default configuration workbook_properties must be a named list"
  )

})

test_that("error if default workbook format is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_default4.yaml")
    ),
    "Default configuration workbook_format must be a named list"
  )

})

test_that("error if aftables cannot find custom config requested by user", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_config.yaml"),
      config_name = "MISSING_custom"
    ),
    "does not contain key `MISSING_custom`"
  )

})

test_that("error if custom config not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_custom.yaml"),
      config_name = "custom"
    ),
    "Configuration key custom must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_custom2.yaml"),
      config_name = "custom"
    ),
    "Configuration key custom must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

})

test_that("error if custom workbook properties is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_custom3.yaml"),
      config_name = "custom"
    ),
    "Custom configuration workbook_properties must be a named list"
  )

})

test_that("error if custom workbook format is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_custom4.yaml"),
      config_name = "custom"
    ),
    "Custom configuration workbook_format must be a named list"
  )

})

test_that(".process_config correctly combines config options", {

  # User + default + custom config ---------------------------------------------

  user_config <- list(
    workbook_properties = list(
      author = "user author",
      title = NULL,
      keywords = c("user", "keywords")
    ),
    workbook_format = list()
  )

  config <- .process_config(
    user_config = user_config,
    config_path = testthat::test_path("configs/test_valid_config.yaml"),
    config_name = "custom"
  )

  expected_config <- list(
    workbook_properties = list(
      author = "user author",
      title = "default config title",
      keywords = c("user", "keywords"),
      subject = "default config subject",
      category = "default config category",
      comments = "default config comments"
    ),
    workbook_format = list(
      base_font_name = "default font",
      base_font_size = 1,
      table_header_size = 1,
      sheet_header_size = 50,
      cellwidth_default = 1,
      cellwidth_wider = 1,
      nchar_break = 1
    )
  )

  expect_equal(config, expected_config)


  # User + default config ------------------------------------------------------

  config_default <- .process_config(
    user_config = user_config,
    config_path = testthat::test_path("configs/test_valid_config.yaml"),
    config_name = NULL
  )

  expected_config_default <- list(
    workbook_properties = list(
      author = "user author",
      title = "default config title",
      keywords = c("user", "keywords"),
      subject = "default config subject",
      category = "default config category",
      comments = "default config comments"
    ),
    workbook_format = list(
      base_font_name = "default font",
      base_font_size = 1,
      table_header_size = 1,
      sheet_header_size = 1,
      cellwidth_default = 1,
      cellwidth_wider = 1,
      nchar_break = 1
    )
  )

  expect_equal(config_default, expected_config_default)


  # Custom config only ---------------------------------------------------------

  config_custom <- .process_config(
    user_config = list(workbook_properties = list(), workbook_format = list()),
    config_path = testthat::test_path("configs/test_valid_config2.yaml"),
    config_name = "custom"
  )

  expected_config_custom <- list(
    workbook_properties = list(
      author = "custom config author"
    ),
    workbook_format = list(
      sheet_header_size = 50
    )
  )

  expect_equal(config_custom, expected_config_custom)


  # user only ------------------------------------------------------------------

  config_user <- .process_config(
    user_config = user_config,
    config_path = NULL,
    config_name = NULL
  )

  expected_config_user <- list(
    workbook_properties = list(
      author =  "user author",
      keywords = c("user", "keywords")
    ),
    workbook_format = list()
  )

  expect_equal(config_user, expected_config_user)

})


# Test .validate_config --------------------------------------------------------

test_that("error when config entries have invalid names or in wrong place", {

  expect_warning(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_config.yaml")
    ),
    "Some entries in your config file could not be processed."
  )

})

test_that("error when entries apart from keywords have more than 1 value", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_lengths_config.yaml"),
      config_name = "wrong-lengths"
    ),
    "Config entries must contain only one value apart from keywords. Please check your config file."
  )
})

test_that("error when values in config.yaml are wrong datatype (character/numeric/list)", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_wrong_datatypes_config.yaml"),
      config_name = "wrong-datatypes"
    ),
    "Please review the following invalid config entries"
  )

})
