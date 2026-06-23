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
#' decimal places and commas as thousand separators. Use this function before
#' the data frame is passed to the aftables::create_aftable function.
#'
#' @param table Required data frame. Data frame to be passed into
#' aftables::create_aftable function. No default.
#' @param numeric_columns Required character vector containing names of numeric
#' columns or quoted tidyselect pattern determining columns to be processed
#' without number formatting. No default.
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
#' # Prevent specific columns being formatted with thousand separators and decimal places
#' table_1_df <- prevent_number_formatting(table_1_df,
#'                                          numeric_columns = c("Date", "Date2"))
#'
#' # Prevent all numeric columns being formatted with thousand separators and decimal places
#' table_1_df <- prevent_number_formatting(table_1_df,
#'                                          numeric_columns = "is.numeric")
#'}
#' @export

prevent_number_formatting <- function(table,
                                      numeric_columns) {

  if (all(numeric_columns %in% names(table))) {
    output <-
      table |>
      mutate(
        across({{ numeric_columns }},
               ~ {
                  attr(.x, "non-numeric") <- TRUE
                  .x})
      )
  } else {
    output <-
      table |>
      mutate(
        across(where({{ numeric_columns }}),
               ~ {
                  attr(.x, "non-numeric") <- TRUE
                  .x})
      )
  }

  output

}
