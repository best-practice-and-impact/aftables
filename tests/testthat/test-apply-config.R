test_that("no config is applied without config.yaml or function arguments", {
  expect_warning(
    wb <-
      generate_workbook(
        as_aftable(demo_df),
        config_path = NULL
      ),
    "Some of the recommended workbook properties are missing."
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # only 4 properties should be set (by openxlsx2 when wbWorkbook object created)
  expect_equal(
    names(wb_properties),
    c("creator", "datetime_created", "datetime_modified", "modifier")
  )

})

test_that("minimum properties are applied correctly via arguments", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    author = "aftables author",
    title = "aftables test workbook arguments",
    keywords =  c("aftablesexample", "aftablesdemonstration",
                  "aftablesconfig.yaml"),
    config_path = NULL
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # only 6 properties should be set
  expect_equal(
    names(wb_properties),
    c("creator",
      "datetime_created",
      "datetime_modified",
      "modifier",
      "title",
      "keywords")
  )

  # minimum properties
  expect_equal(wb_properties["creator"],
               c("creator" = "aftables author"))
  expect_equal(wb_properties["title"],
               c("title" = "aftables test workbook arguments"))
  expect_equal(wb_properties["keywords"],
               c("keywords" = "aftablesexample, aftablesdemonstration, aftablesconfig.yaml"))

  # if modifier is blank it is populated with value for author
  expect_equal(wb_properties["modifier"],
               c("modifier" = "aftables author"))

})

test_that("properties from config.yaml are ignored when properties arguments are set", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    author = NULL,
    title = "title fun argument",
    keywords =  c("keywords", "fun", "argument"),
    config_path = testthat::test_path("configs/test_custom_config.yaml"),
    config_name = "custom-config"
  )

  wb_properties <- openxlsx2::wb_get_properties(wb)

  # Properties from function arguments
  expect_equal(wb_properties["title"],
               c("title" = "title fun argument"))
  expect_equal(wb_properties["keywords"],
               c("keywords" = "keywords, fun, argument"))

  # Properties from custom config
  expect_equal(wb_properties["creator"],
               c("creator" = "Author custom config"))

  # Properties from default config
  expect_equal(wb_properties["subject"],
               c("subject" = "Subject default config test"))

})

test_that("Default cell formats are set properly", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    config_path = testthat::test_path("configs/test_font_config.yaml")
  )

  wb_fonts <- wb$styles_mgr$font

  wb_xf <- wb$styles_mgr$xf

  # Sheet heading
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Table_1", "A1") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Arial\"/>")) # Arial
  expect_true(stringr::str_detect(cell_format, "<sz val=\"16\"/>")) # size 16

  # Sheet subheading
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Cover", "A2") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Arial\"/>")) # Arial
  expect_true(stringr::str_detect(cell_format, "<sz val=\"14\"/>")) # size 14

  # table header
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Table_1", "A8") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Arial\"/>")) # Arial
  expect_true(stringr::str_detect(cell_format, "<sz val=\"12\"/>")) # size 12

  # default font Arial size 12
  # get properties with wb_get_base_font
  base_font <- openxlsx2::wb_get_base_font(wb)

  expect_equal(base_font$size$val, "12")

  expect_equal(base_font$name$val, "Arial")

})

test_that("Cell formats are set properly from default and custom configs combined", {

  wb <-
    generate_workbook(
      as_aftable(demo_df),
      config_path = testthat::test_path("configs/test_mixed_font_config.yaml"),
      config_name = "workbook1"
    )

  wb_fonts <- wb$styles_mgr$font

  wb_xf <- wb$styles_mgr$xf

  # Sheet heading
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Table_1", "A1") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Calibri\"/>")) # custom Calibri
  expect_true(stringr::str_detect(cell_format, "<sz val=\"20\"/>")) # custom size 20

  # Sheet subheading
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Cover", "A2") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Calibri\"/>")) # custom Calibri
  expect_true(stringr::str_detect(cell_format, "<sz val=\"18\"/>")) # custom size 18

  # table header
  xf_font_id <- openxlsx2::wb_get_cell_style(wb, "Table_1", "A8") |> as.integer()

  font_id <- (wb_xf |> filter(id == xf_font_id))$name

  font_id <- sub(".*fontId=\"([0-9]+)\".*", "\\1", font_id) |> as.numeric()

  cell_format <- (wb_fonts |> filter(id == font_id))$name

  expect_true(stringr::str_detect(cell_format, "<b val=\"1\"/>")) # bold
  expect_true(stringr::str_detect(cell_format, "<name val=\"Calibri\"/>")) # custom Calibri
  expect_true(stringr::str_detect(cell_format, "<sz val=\"16\"/>")) # custom size 16

  # default font Calibri size 12
  # get properties with wb_get_base_font
  base_font <- openxlsx2::wb_get_base_font(wb)

  expect_equal(base_font$size$val, "12")

  expect_equal(base_font$name$val, "Calibri")

})

test_that("Default column widths are set", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    config_path = testthat::test_path("configs/test_widths_config.yaml")
  )

  # column widths of worksheet Table_1 are standard for columns 1 to 5 and 7
  # columns 6 is a wide column
  col_widths <- wb$worksheets[[4]]$cols_attr

  expect_true(stringr::str_detect(col_widths[1], "min=\"1\" max=\"5\"") &&
                stringr::str_detect(col_widths[1], "width=\"16.555\""))

  expect_true(stringr::str_detect(col_widths[2], "min=\"6\" max=\"6\"") &&
                stringr::str_detect(col_widths[2], "width=\"32.555\""))

  expect_true(stringr::str_detect(col_widths[3], "min=\"7\" max=\"7\"") &&
                stringr::str_detect(col_widths[3], "width=\"16.555\""))

})

test_that("Custom column widths are set from default and custom configs combined", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    config_path = testthat::test_path("configs/test_custom_widths_config.yaml"),
    config_name = "workbook1"
  )

  # column widths of worksheet Table_1 are standard for columns 1 to 5 and 7
  # columns 6 is a wide column
  col_widths <- wb$worksheets[[4]]$cols_attr

  expect_true(stringr::str_detect(col_widths[1], "min=\"1\" max=\"5\"") &&
                stringr::str_detect(col_widths[1], "width=\"10.555\""))

  expect_true(stringr::str_detect(col_widths[2], "min=\"6\" max=\"6\"") &&
                stringr::str_detect(col_widths[2], "width=\"14.555\""))

  expect_true(stringr::str_detect(col_widths[3], "min=\"7\" max=\"7\"") &&
                stringr::str_detect(col_widths[3], "width=\"10.555\""))

})


test_that("Setting nchar_break different to default forces column widths to change", {

  wb <- generate_workbook(
    as_aftable(demo_df),
    config_path = testthat::test_path("configs/test_nchar_break_config.yaml")
  )

  # nchar_break is set to 8
  # Column headers for columns 2 to 6 are greater than are 8 characters
  # so they are  widened
  # columns 1 and 7 are less than or equal to 8 characters so are not widened
  col_widths <- wb$worksheets[[4]]$cols_attr

  expect_true(stringr::str_detect(col_widths[1], "min=\"1\" max=\"1\"") &&
                stringr::str_detect(col_widths[1], "width=\"16.555\""))

  expect_true(stringr::str_detect(col_widths[2], "min=\"2\" max=\"6\"") &&
                stringr::str_detect(col_widths[2], "width=\"32.555\""))

  expect_true(stringr::str_detect(col_widths[3], "min=\"7\" max=\"7\"") &&
                stringr::str_detect(col_widths[3], "width=\"16.555\""))

})
