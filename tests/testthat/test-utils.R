test_that("vector elements are converted to sentence form", {
  expect_equal(.vector_to_sentence(LETTERS[1]), "A")
  expect_equal(.vector_to_sentence(LETTERS[1:2]), "A and B")
  expect_equal(.vector_to_sentence(LETTERS[1:3]), "A, B and C")
})


test_that(".determine functions correctly identify cells", {

  determine_functions_df <- data.frame(

    # Columns will all empty cells should remain the same
    empty = c(rep("", 8), NA),

    # Numeric columns should stay numeric
    numeric1 = c(1, 5, 26.25, 123, -15677, 23.45, 67, 45, NA),
    numeric2 = c(1:7, -8001L, NA),

    # Character columns containing only numbers should be converted to numeric
    number_character = c("1", "-1.345", "2322.456", "12.44", NA, "- 23.546",
                         " - 23", " 232.3 ", "0.12 "),

    # Character columns with mixed numbers and text in same cell should not be
    # converted to numeric
    mixed_text_numbers = c("12", "-15.6", "-13.4 [u]", " 1800.1 [note2]", NA,
                           "[c]", "13.7[u][bc] ", "78.6 [note 1][Note 4]",
                           "12.7 [note1, note2]"),

    # Character columns with numbers and note cells should be converted to
    # numeric
    number_notes = c("1", " -3.3", "-2834.459 ", "-23,456", "[c]",
                     "[-0.3cs] [g f]", "[ as][f]", "[dvb12,v.]", "7.8"),

    # Columns containing money should be converted to numeric
    # Columns with money and notes/numbers should be converted to numeric
    currency_single = c(NA, "-£12.3", "£13.556", "£0.6", "£-13", "£15001",
                        "£19,000.12", " £ 12.3", "£- 12 "), # currency, all £
    currency_multiple = c("£12.3", NA, "-£ 13.559", "$0.6", "£13", " £15,001",
                          " €19,000.12", " £12.3",
                          "12 "), # Currency, mixed symbols
    currency_notes = c("£12.3", NA, "£ 13.55", "-$0.6", " [-£0.2] [cvb] ",
                       " £15,001", " €19,000.12", " £12.3",
                       "12 $"), # Currency and notes

    # Columns containing only notes should stay as character
    notes = c("[a]", "[abv] [efg]", " [note 1]", "[c]  ", "[abc][efg]",
              "[ab cd]", "[a][b][c]", "[a$]", "[12]")

  )

  expected_notes_cells <- data.frame(
    empty = rep(FALSE, 9),
    numeric1 = rep(FALSE, 9),
    numeric2 = rep(FALSE, 9),
    number_character = rep(FALSE, 9),
    mixed_text_numbers = c(rep(FALSE, 5), TRUE, rep(FALSE, 3)),
    number_notes = c(rep(FALSE, 4), rep(TRUE, 4), FALSE),
    currency_single = rep(FALSE, 9),
    currency_multiple = rep(FALSE, 9),
    currency_notes = c(rep(FALSE, 4), TRUE, rep(FALSE, 4)),
    notes = rep(TRUE, 9)
  )

  expected_numeric_cells <- data.frame(
    empty = rep(FALSE, 9),
    numeric1 = c(rep(TRUE, 8), FALSE),
    numeric2 = c(rep(TRUE, 8), FALSE),
    number_character = c(rep(TRUE, 4), FALSE, rep(TRUE, 4)),
    mixed_text_numbers = c(rep(TRUE, 2), rep(FALSE, 7)),
    number_notes = c(rep(TRUE, 4), rep(FALSE, 4), TRUE),
    currency_single = rep(FALSE, 9),
    currency_multiple = c(rep(FALSE, 8), TRUE),
    currency_notes = rep(FALSE, 9),
    notes = rep(FALSE, 9)
  )

  expected_currency <- data.frame(
    empty = rep(FALSE, 9),
    numeric1 = rep(FALSE, 9),
    numeric2 = rep(FALSE, 9),
    number_character = rep(FALSE, 9),
    mixed_text_numbers = rep(FALSE, 9),
    number_notes =  rep(FALSE, 9),
    currency_single = c(FALSE, rep(TRUE, 8)),
    currency_multiple = c(TRUE, FALSE, rep(TRUE, 6), FALSE),
    currency_notes = c(TRUE, FALSE, rep(TRUE, 2), FALSE, rep(TRUE, 4)),
    notes = rep(FALSE, 9)
  )

  expected_empty <- data.frame(
    empty = rep(TRUE, 9),
    numeric1 = c(rep(FALSE, 8), TRUE),
    numeric2 = c(rep(FALSE, 8), TRUE),
    number_character = c(rep(FALSE, 4), TRUE, rep(FALSE, 4)),
    mixed_text_numbers = c(rep(FALSE, 4), TRUE, rep(FALSE, 4)),
    number_notes =  rep(FALSE, 9),
    currency_single = c(TRUE, rep(FALSE, 8)),
    currency_multiple = c(FALSE, TRUE, rep(FALSE, 7)),
    currency_notes = c(FALSE, TRUE, rep(FALSE, 7)),
    notes = rep(FALSE, 9)
  )

  notes_cells <- .determine_note_cells(determine_functions_df)
  expect_equal(notes_cells, expected_notes_cells)

  numeric_cells <- .determine_numeric_cells(determine_functions_df)
  expect_equal(numeric_cells, expected_numeric_cells)

  currency_cells <- .determine_currency_cells(determine_functions_df)
  expect_equal(currency_cells, expected_currency)

  empty_cells <- .determine_empty_cells(determine_functions_df)
  expect_equal(empty_cells, expected_empty)

  # numeric_columns identifies all columns which need to be output as numeric,
  # including columns containing currency and mix of numbers/currency and notes
  numeric_columns <-
    .determine_table_datatypes(determine_functions_df)$numeric_columns
  expect_equal(
    numeric_columns,
    c("numeric1", "numeric2", "number_character", "number_notes",
      "currency_single", "currency_multiple", "currency_notes")
  )

})


test_that("table cleaning functions work as intended", {

  # .replace_currency_units ----------------------------------------------------

  currency_df <- data.frame(
    col1 = c("£12.30", " 13$ ", "   €123,123.1234"),
    col2 = c(" €19,000.12", "£12.30", "£12 ")
  )

  expected_currency_df <- data.frame(
    col1 = c("12.30", " 13 ", "   123,123.1234"),
    col2 = c(" €19,000.12", "£12.30", "£12 ")
  )

  currency_df <- .replace_currency_units(currency_df, "col1")

  expect_equal(currency_df, expected_currency_df)


  # .clean_numeric_data --------------------------------------------------------

  df <- data.frame(
    col1 = c("12.30", NA, " 13 ", "   123,123.1234"),
    col2 = c(" 19,000.12", "12.30", "12 ", NA),
    col3 = c(123, 235, NA, 12.4),
    col4 = c("  123", "12,001 ", NA, " 12.1")
  )

  cleaned_df_expected <- data.frame(
    col1 = c(12.30, NA, 13, 123123.1234),
    col2 = c(19000.12, 12.30, 12, NA),
    col3 = c(123, 235, NA, 12.4),
    col4 = c("  123", "12,001 ", NA, " 12.1")
  )


  # Clean 3 of the 4 columns
  # Col 1 and 2 should be converted to numeric. Col 3 and 4 should remain the
  # same.
  cleaned_df <- .clean_numeric_data(df, c("col1", "col2", "col3")
  )

  expect_equal(cleaned_df, cleaned_df_expected)


})
