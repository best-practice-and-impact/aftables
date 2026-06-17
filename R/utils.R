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
#' Helper function for numeric columns containing dates
#'
#' Prevent aftables formatting columns containing dates as years as numbers with
#' decimal places and comma separators. Use this function before the data frame
#' is passed to the aftables::create_aftable function.
#'
#' @param table Required data frame. Data frame to be passed into
#' aftables::create_aftable function. No default.
#' @param dates_columns Required character vector containing names of columns in
#' table to be processed without number formatting. No default.

years_helper <- function(table,
                         dates_columns) {

  output <-
    table |>
    dplyr::mutate(across(dates_columns, ~
                           magrittr::set_attr(.x,
                                              "numeric_years",
                                              TRUE)))

  output

}
