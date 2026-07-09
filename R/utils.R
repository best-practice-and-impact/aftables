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

#' Helper function to specify how aftables should display numeric data
#'
#' Control how aftables formats data as numbers with decimal places and commas
#' as thousand separators. This function can be used to overwrite the default
#' behaviour of aftables, which normally determines the number of decimal places
#' required automatically from the data in each numeric column, and adds
#' thousand separators to numeric columns. A key use case for this function is
#' to prevent aftables from displying calendar or financial years as numbers
#' with thousand separators, but it can be used with any numeric data. Use this
#' function before the data frame is passed to the aftables::create_aftable
#' function.
#'
#' @param table Required data frame. Data frame to be passed into
#'   aftables::create_aftable function. No default.
#' @param columns Required character vector containing names of columns or
#'   tidyselect pattern determining columns to be processed with specified
#'   number formatting. No default.
#' @param decimal_places Required numeric value specifying decimal places to
#'   apply to data in specified columns. Default (NULL) is for aftables to
#'   automatically determine number of decimal places from the data.
#' @param thousand_separators Required logical value whether data in specified
#'   columns should be formatted with thousand separators. Default (NULL) is for
#'   aftables to automatically determine whether to use thosands separators.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' set.seed(1066)
#'
#' table_1_df <- data.frame(
#'   Category = LETTERS[1:10],
#'   Date = 2001:2010,
#'   Date2 = 2001:2010,
#'   "Count" = abs(round(rnorm(10), 3) * 1e3),
#'   "Population" = abs(round(rnorm(10), 5) * 1e5),
#'   check.names = FALSE
#' ) |>
#'   mutate(Percentage = Count/Population * 100)
#'
#' # Specify removing thousand separators for Date and Date2 columns
#' table_1_df_formatted <- table_1_df |>
#'   number_formatter(
#'     columns = c(Date, Date2),
#'     thousand_separators = FALSE
#'   )
#'
#' # number_formatter columns argument also accepts tidyselect expressions
#' table_1_df_formatted_tidyselect <- table_1_df |>
#' number_formatter(
#'   columns = tidyselect::starts_with("Date"),
#'   thousand_separators = FALSE
#' )
#'
#' # The default precision for "Percentage" column is 7 decimal places
#' # This is overruled by number_formatter to display the data in Excel workbook
#' # to 2 decimal places
#' table_1_df_formatted_2dp <- table_1_df |>
#'   number_formatter(
#'     columns = "Percentage",
#'     decimal_places = 2
#'   )
#'
#' # number_formatter can be used multiple times to set different number formats
#' table_1_df_multiple_formats <- table_1_df |>
#'   number_formatter(
#'     columns = "Percentage",
#'     decimal_places = 2
#'   ) |>
#'   number_formatter(
#'     columns = tidyselect::starts_with("Date"),
#'     thousand_separators = FALSE
#'   )
#'
#' }
#' @export

number_formatter <- function(table,
                             columns,
                             decimal_places = NULL,
                             thousand_separators = NULL) {

  # error check decimal_places and thousand_separators are length 1
  if (length(decimal_places) > 1) {
    stop("`decimal_places` must be of length 1.", call. = FALSE)
  }

  if (!is.null(decimal_places) &&
        !((is.numeric(decimal_places)) && !is.na(decimal_places))) {
    stop("`decimal_places` must be numeric.", call. = FALSE)
  }

  if (length(thousand_separators) > 1) {
    stop("`thousand_separators` must be of length 1.", call. = FALSE)
  }

  if (!is.null(thousand_separators) &&
        !((is.logical(thousand_separators) && !is.na(thousand_separators)))) {
    stop("`thousand_separators` must be TRUE or FALSE.", call. = FALSE)
  }

  output <- table |>
    mutate(
      across(
        .cols = {{ columns }},
        .fns = \(x) {
          attr(x, "aftables_decimal_places") <- decimal_places
          x
        }
      ),
      across(
        .cols = {{ columns }},
        .fns = \(x) {
          attr(x, "aftables_thousand_separators") <- thousand_separators
          x
        }
      )
    )

  output
}
