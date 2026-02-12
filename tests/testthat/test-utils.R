# test_that("vector elements are converted to sentence form", {
#   expect_equal(.vector_to_sentence(LETTERS[1]), "A")
#   expect_equal(.vector_to_sentence(LETTERS[1:2]), "A and B")
#   expect_equal(.vector_to_sentence(LETTERS[1:3]), "A, B and C")
# })
#
# determine_functions_df <- data.frame(
#   notes = c("[a][b]", "[c]", "[d]", "[e]"),
#   numbers = c(1.1, 1.2, 1.3, 1.4),
#   currencies = c("£1.1", "$1.1", "¥1.3", "€1.2"),
#   empty = c("", "", NA, NA),
#   mixed_numeric = c("1.1", "1.2", "1.3", "[note]"),
#   mixed_currencies = c("€1.1", "$1.2", "¥1.3", "[note]"),
#   numeric_characters = c("1.1", "1.2", "1.2 [note]", "1.5")
# )
#
# test_that(".determine functions correctly identify cells", {
#
#   notes_cells <- .determine_note_cells(determine_functions_df)$notes
#   numeric_cells <- .determine_numeric_cells(determine_functions_df)$numbers
#   currency_cells <- .determine_currency_cells(determine_functions_df)$currencies
#   empty_cells <- .determine_empty_cells(determine_functions_df)$empty
#
#   expect_all_true(notes_cells)
#   expect_all_true(numeric_cells)
#   expect_all_true(currency_cells)
#   expect_all_true(empty_cells)
#
#   numeric_columns <- .determine_table_datatypes(determine_functions_df)$numeric_columns
#   table_note_cells <- .determine_table_datatypes(determine_functions_df)$note_cells
#   table_numeric_cells <- .determine_table_datatypes(determine_functions_df)$numeric_cells
#   table_currency_cells <- .determine_table_datatypes(determine_functions_df)$currency_cells
#
#   # notes cells only valid in mixed columns
#   expect_equal(
#     table_note_cells,
#     as.matrix(
#       data.frame(
#         notes = rep(FALSE, 4),
#         numbers = rep(FALSE, 4),
#         currencies = rep(FALSE, 4),
#         empty = rep(FALSE, 4),
#         mixed_numeric = c(rep(FALSE, 3), TRUE),
#         mixed_currencies = c(rep(FALSE, 3), TRUE),
#         numeric_characters = rep(FALSE, 4)
#       )
#     )
#   )
#
#   expect_equal(
#     table_numeric_cells,
#     as.matrix(
#       data.frame(
#         notes = rep(FALSE, 4),
#         numbers = rep(TRUE, 4),
#         currencies = rep(FALSE, 4),
#         empty = rep(FALSE, 4),
#         mixed_numeric = c(rep(TRUE, 3), FALSE),
#         mixed_currencies = rep(FALSE, 4),
#         numeric_characters = c(TRUE, TRUE, FALSE, TRUE)
#       )
#     )
#   )
#
#   expect_equal(
#     table_currency_cells,
#     as.matrix(
#       data.frame(
#         notes = rep(FALSE, 4),
#         numbers = rep(FALSE, 4),
#         currencies = rep(TRUE, 4),
#         empty = rep(FALSE, 4),
#         mixed_numeric = rep(FALSE, 4),
#         mixed_currencies = c(rep(TRUE, 3), FALSE),
#         numeric_characters = rep(FALSE, 4)
#       )
#     )
#   )
#
#   expect_equal(
#     numeric_columns,
#     c("numbers", "currencies", "mixed_numeric", "mixed_currencies")
#   )
#
# })
#
# test_that("table cleaning functions work as intended", {
#   clean_functions_df <- determine_functions_df
#
#   table_currency_cells <-
#     .determine_table_datatypes(determine_functions_df)$currency_cells
#   table_numeric_cells <-
#     .determine_table_datatypes(determine_functions_df)$numeric_cells
#
#   clean_functions_df[table_currency_cells] <-
#     .replace_currency_units(clean_functions_df, table_currency_cells)
#
#   # currency units are removed from currency cells
#   expect_equal(
#     clean_functions_df$currencies,
#     c("1.1", "1.1", "1.3", "1.2")
#   )
#
#   expect_equal(
#     clean_functions_df$mixed_currencies,
#     c("1.1", "1.2", "1.3", "[note]")
#   )
#
#   clean_functions_df <-
#     .clean_numeric_data(clean_functions_df, table_numeric_cells)
#
#   # numbers in numeric columns are numeric
#   expect_equal(
#     clean_functions_df$numbers,
#     c(1.1, 1.2, 1.3, 1.4)
#   )
#
#   expect_equal(
#     clean_functions_df$currencies,
#     c(1.1, 1.1, 1.3, 1.2)
#   )
#
#   # numbers in mixed columns are still characters
#   expect_equal(
#     clean_functions_df$mixed_numeric,
#     c("1.1", "1.2", "1.3", "[note]")
#   )
#
#   expect_equal(
#     clean_functions_df$mixed_currencies,
#     c("1.1", "1.2", "1.3", "[note]")
#   )
#
# })
