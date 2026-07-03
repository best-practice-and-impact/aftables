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

#' Helper function for more control over how aftables displays numeric data
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
#' table_1_df_non_numeric_dates <- prevent_number_formatting(
#'   table_1_df,
#'   numeric_columns = c("Date", "Date2")
#' )
#'
#' # Prevent all numeric columns being formatted with thousand separators and decimal places
#' table_1_df_all_non_numeric <- prevent_number_formatting(
#'   table_1_df,
#'   numeric_columns = which(is.numeric)
#' )
#'}
#' @export

prevent_number_formatting <- function(table,
                                      numeric_columns) {

  output <- format_numbers_helper(
    table = table,
    columns = {{ numeric_columns }},
    decimal_places = 0,
    thousand_separators = FALSE
  )

  output
}

#' Helper function to specify how aftables should display numeric data
#'
#' Control how aftables formats data in numeric columns as numbers with
#' decimal places and commas as thousand separators. This function can be used
#' to overwrite the default behaviour of aftables, which normally determines the
#' number of decimal places required from the data in each numeric column, and
#' adds thousand separators to numeric columns. Use this function before the
#' data frame is passed to the aftables::create_aftable function.
#'
#' @param table Required data frame. Data frame to be passed into
#' aftables::create_aftable function. No default.
#' @param columns Required character vector containing names of numeric
#' columns or tidyselect pattern determining columns to be processed with
#' specified number formatting. No default.
#' @param decimal_places Required numeric vector specifying decimal places to
#' apply to data in specified columns. No default.
#' @param thousand_separators Required logical vector specifying whether data in
#' specified columns should be formatted with thousand separators. No default.
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
#' # Specify decimal places and thousand separators for Date, Date2 and Numeric decimal columns
#' table_1_df_formatted <- format_numbers_helper(
#'   table = table_1_df,
#'  columns = c("Date","Date2", "Numeric decimal"),
#'   decimal_places = c(0, 0, 5),
#'   thousand_separators = c(FALSE, FALSE, TRUE)
#' )
#'}
#' @export

format_numbers_helper <- function(table,
                                  columns,
                                  decimal_places,
                                  thousand_separators) {

  # Prevent mutate error if columns don't exist
  if (typeof(columns) != "closure" && !columns %in% names(table)) {
    stop("All columns must be in table.", call. = FALSE)
  }

  if (typeof(columns) != "closure") {
    names(decimal_places) <- columns
    names(thousand_separators) <- columns
  }

  output <-
    table |>
    mutate(
      across(
        .cols = {{ columns }},
        .fns = \(x) {
          ifelse(
            typeof(columns) == "closure", # tidyselect selector
            attr(x, "decimal_places") <- decimal_places,
            attr(x, "decimal_places") <- decimal_places[dplyr::cur_column()]
          )
          x
        }
      ),
      across(
        .cols = {{ columns }},
        .fns = \(x) {
          ifelse(
            typeof(columns) == "closure", # tidyselect selector
            attr(x, "thousand_separators") <- thousand_separators,
            attr(x, "thousand_separators") <-
              thousand_separators[dplyr::cur_column()]
          )
          x
        }
      )
    )

  output
}
