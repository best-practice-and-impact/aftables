# Test .process_config ---------------------------------------------------------

test_that("error if config file doesn't exist", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      author = NULL,
      title = NULL,
      keywords = NULL,
      config_path = testthat::test_path("DOES_NOT_EXIST.yaml"),
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
      config_path = testthat::test_path("test_missing_aftable_config.yaml"),
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
      config_path = testthat::test_path("test_missing_default_config.yaml"),
      config_name = NULL
    ),
    "does not contain a default aftables configuration and a custom key is not being used"
  )

})

test_that("error if default config not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("test_wrong_default.yaml")
    ),
    "Default configuration key must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("test_wrong_default2.yaml")
    ),
    "Default configuration key must be a named list. It can only contain keys `workbook_properties` and `workbook_format`."
  )

})

test_that("error if default workbook properties is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("test_wrong_default3.yaml")
    ),
    "Configuration Default:workbook_properties must be a named list"
  )

})

test_that("error if default workbook format is not a named list", {

  expect_error(
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("test_wrong_default4.yaml")
    ),
    "Configuration Default:workbook_format must be a named list"
  )

})

test_that("error if aftables cannot find custom config requested by user", {

  expect_error(
    expect_warning(
      generate_workbook(
        as_aftable(demo_df),
        config_path = testthat::test_path("test_config.yaml"),
        config_name = "MISSING_custom"
      ),
      "The config file contains values identical to the aftables example config. Please check your config file."
    ),
    "does not contain key `MISSING_custom`"
  )

})


# Test .validate_config --------------------------------------------------------

test_that("error when config entries have invalid names or in wrong place", {

  expect_warning(
    expect_warning(
      generate_workbook(
        as_aftable(demo_df),
        config_path = testthat::test_path("test_wrong_config.yaml")
      ),
      "Your config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Some entries in your config file could not be processed."
  )

})

test_that("error when entries apart from keywords have more than 1 value", {

  expect_error(
    expect_warning(
      generate_workbook(
        as_aftable(demo_df),
        config_path = testthat::test_path("test_config.yaml"),
        config_name = "wrong-lengths"
      ),
      "Your config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Config entries must contain only one value apart from keywords. Please check your config file."
  )
})

test_that("error when values in config.yaml are wrong datatype (character/numeric/list)", {

  expect_error(
    expect_warning(
      generate_workbook(
        as_aftable(demo_df),
        config_path = testthat::test_path("test_config.yaml"),
        config_name = "wrong-datatypes"
      ),
      "The config file contains values identical to the aftables example config. Please check your config file."
    ),
    "Please review the following invalid config entries"
  )

})
