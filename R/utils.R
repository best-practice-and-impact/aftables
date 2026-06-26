#' Convert a List to A Sentence Form
#' Vectors of 1, 2 and 3 letters become 'A', 'A and B', 'A, B and C'.
#' @noRd
.vector_to_sentence <- function(vector) {
  if (length(vector) > 1) {
    last <- vector[length(vector)]
    not_last <- vector[-length(vector)]
    sentence <- paste(paste(not_last, collapse = ", "), "and", last)
    return(sentence)
  }

  vector
}

#' Helper function for more control over how aftables processes numeric data
#'
#' Prevent aftables formatting data in numeric columns as numbers with
#' decimal places and commas as thousand separators. This function is intended
#' to prevent aftables from formatting calendar or financial years as numbers,
#' but may be used with any numeric data. Use this function before the data
#' frame is passed to the aftables::create_aftable function.
#'
#' @param table Required data frame. Data frame to be passed into
#' aftables::create_aftable function. No default.
#' @param numeric_columns Required character vector containing names of numeric
#' columns or tidyselect pattern determining columns to be processed without
#' number formatting. No default.
#' @examples
#' \dontrun{
#'
#' table_1_df <- data.frame(
#'   Category = LETTERS[1:10],
#'   Date = 2001:2010,
#'   Date2 = 2001:2010,
#'   "Numeric thousands" = abs(round(rnorm(10), 4) * 1e5),
#'   "Numeric decimal" = abs(round(rnorm(10), 5)),
#'   check.names = FALSE
#' )
#'
#' # Prevent specific numeric columns being formatted with thousand separators and decimal places
#' table_1_df_non_numeric_dates <- prevent_number_formatting(table_1_df,
#'                                                           numeric_columns = c("Date", "Date2"))
#'
#' # Prevent all numeric columns being formatted with thousand separators and decimal places
#' table_1_df_all_non_numeric <- prevent_number_formatting(table_1_df,
#'                                                         numeric_columns = which(is.numeric))
#'}
#' @export

prevent_number_formatting <- function(table,
                                      numeric_columns) {

  output <-
    .format_numbers_helper(data = table,
                           columns = {{ numeric_columns }},
                           decimal_places = 0,
                           thousand_separators = FALSE)

  attr(output, "aftables_prevent_number_formatting") <- TRUE

  output
}

.format_numbers_helper <- function(data,
                                   columns,
                                   decimal_places,
                                   thousand_separators) {
  output <-
    data |>
    mutate(
      across({{ columns }},
             \(x) {
               attr(x, "decimal_places") <- decimal_places
               x}),
      across({{ columns }},
             \(x) {
               attr(x, "thousand_separators") <- thousand_separators
               x})
    )

  output
}
