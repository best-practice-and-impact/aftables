test_that("vector elements are converted to sentence form", {
  expect_equal(.vector_to_sentence(LETTERS[1]), "A")
  expect_equal(.vector_to_sentence(LETTERS[1:2]), "A and B")
  expect_equal(.vector_to_sentence(LETTERS[1:3]), "A, B and C")
})

determine_functions_df <- data.frame(notes = c("[a][b]", "[c]", "[d]", "[e]"),
                                     numbers = c(1.1, 1.2, 1.3, 1.4),
                                     currencies = c("£1.1", "$1.1", "¥1.3", "€1.2"),
                                     empty = c("", "", NA, NA),
                                     mixed = c("1.1", "1.2", "1.3", "[note]"))

test_that(".determine functions correctly identify cells", {

  expect_all_true(.determine_note_cells(determine_functions_df)$notes)
  expect_all_true(.determine_numeric_cells(determine_functions_df)$numbers)
  expect_all_true(.determine_currency_cells(determine_functions_df)$currencies)
  expect_all_true(.determine_empty_cells(determine_functions_df)$empty)

  expect_equal(.determine_mixed_columns(determine_functions_df),
               c(FALSE, FALSE, FALSE, FALSE, TRUE))

  expect_equal(.determine_numeric_columns(determine_functions_df),
               c("numbers", "currencies", "mixed"))

})
