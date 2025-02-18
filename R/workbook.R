
#' Generate A Workbook Object From An 'aftable'
#'
#' Populate an 'openxlsx' Workbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx]{saveWorkbook}} from 'openxlsx'
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#'
#' @return A Workbook-class object.
#'
#' @examples
#' # Convert an aftable to a Workbook-class object
#' x <- generate_workbook(demo_aftable)
#' class(x)
#'
#' # As above, using a compliant data.frame and the base pipe
#' y <- demo_df |>
#'   as_aftable() |>
#'   generate_workbook()
#'
#' @export
generate_workbook <- function(aftable) {

  if (!is_aftable(aftable)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  # Create a table_name from tab_title (unique, no spaces, no punctuation)
  aftable[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable[["tab_title"]])))
  aftable[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable[["table_name"]], perl = TRUE)

  # Create workbook, add tabs, cover, contents (required for all workbooks)
  wb <- openxlsx::createWorkbook()
  wb <- .add_tabs(wb, aftable)
  wb <- .add_cover(wb, aftable)
  wb <- .add_contents(wb, aftable)

  # There won't always be a notes tab
  if (any(aftable$sheet_type %in% "notes")) {
    wb <- .add_notes(wb, aftable)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable[aftable$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- .add_tables(wb, aftable, table_name = i)
  }

  wb

}
