
#' Generate A Workbook Object From An 'aftable'
#'
#' Populate an 'openxlsx2' wbWorkbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx2]{wb_save}} from 'openxlsx2'
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#'
#' @return An openxlsx2 Workbook-class object.
#'
#' @examples
#' # Convert an aftable to an openxlsx2 wbWorkbook-class object
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

  if (!is_aftable(aftable$tabs)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  # Create a table_name from tab_title (unique, no spaces, no punctuation)
  aftable$tabs[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable$tabs[["tab_title"]])))
  aftable$tabs[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable$tabs[["table_name"]], perl = TRUE)

  # Create workbook, set base style, add tabs, cover, contents (required for all workbooks)
  wb <- wb_workbook(theme = "Office 2007 - 2010 Theme")
  wb <- .set_workbook_parameters(wb, aftable$properties)
  wb <- .style_workbook(wb)
  wb <- .add_tabs(wb, aftable$tabs)
  wb <- .add_cover(wb, aftable$tabs)
  wb <- .add_contents(wb, aftable$tabs)

  # There won't always be a notes tab
  if (any(aftable$tabs$sheet_type %in% "notes")) {
    wb <- .add_notes(wb, aftable$tabs)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable$tabs[aftable$tabs$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- .add_tables(wb, aftable$tabs, table_name = i)
  }

  wb

}
