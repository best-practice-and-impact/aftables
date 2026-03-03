#' Generate A Workbook Object From An 'aftable'
#'
#' Populate an 'openxlsx2' wbWorkbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx2]{wb_save}} from 'openxlsx2'
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#' @param author Optional character string to set the workbook author.
#' Default NULL.
#' @param title Optional character string to set the workbook title.
#' Default NULL.
#' @param keywords Optional character vector to set the workbook keywords.
#' Default NULL.
#' @param config_path Optional character string containing path to config file.
#' Defaults to config.yaml file located in working directory.
#' @param config_name Optional character string specifying which configuration
#'   to use from config file. Default NULL.
#'
#' @return An openxlsx2 wbWorkbook-class object.
#'
#' @details
#'
#' Analysis Function guidance advises workbooks should have the author, title,
#' keywords and language document properties completed. `aftables` provides
#' functionality to set the author, title and keywords properties.
#' See [Releasing statistics in spreadsheets](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/#section-15)
#' for more information including how to set the workbook language.
#'
#' A config file can be used to set workbook properties and formatting. See
#' \code{\link[aftables]{create_config_yaml}} for details of how to create a
#' config.yaml file. If author, title or keywords are provided in both the
#' `config.yaml` file and in the `generate_workbook` arguments, the values provided in the
#' function arguments are preferred to those provided in the `config.yaml` file.
#'
#' @examples
#' # Convert an aftable to an openxlsx2 wbWorkbook-class object
#' # Setting the minimum workbook properties as function arguments
#' \dontrun{
#' example_workbook <- generate_workbook(
#'   demo_aftable,
#'   author = "Example author",
#'   title = "example workbook",
#'   keywords = c("keyword1", "keyword2", "keyword3")
#' )}
#'
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' \dontrun{
#' openxlsx2::wb_get_properties(example_workbook)}
#'
#' # Save the workbook with openxlsx2::wb_save
#' \dontrun{
#' openxlsx2::wb_save(example_workbook, "example_workbook.xlsx")}
#'
#' # Using config.yaml file to set workbook properties and edit text and cell
#' # formatting
#' \dontrun{
#' example_workbook2 <- generate_workbook(
#'   demo_aftable,
#'   config_path = system.file("ext-data", "config.yaml", package = "aftables"),
#'   config_name = "workbook1"
#'   )}
#'
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' \dontrun{
#' openxlsx2::wb_get_properties(example_workbook2)}
#'
#' @export
generate_workbook <- function(aftable,
                              author = NULL,
                              title = NULL,
                              keywords = NULL,
                              config_path = "config.yaml",
                              config_name = NULL) {

  if (!is_aftable(aftable)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  if (!is.null(author) && !rlang::is_scalar_character(author)) {
    stop("author must be a character vector of length 1", call. = FALSE)
  }

  if (!is.null(title) && !rlang::is_scalar_character(title)) {
    stop("title must be a character vector of length 1", call. = FALSE)
  }

  if (!is.null(keywords) && !is.character(keywords)) {
    stop("keywords must be a character vector", call. = FALSE)
  }

  user_config <- list(
    workbook_properties = list(
      author = author,
      title = title,
      keywords = keywords
    ),
    workbook_format = list()
  )

  config <- .process_config(user_config, config_path, config_name)

  workbook_properties <- config$workbook_properties
  workbook_format <- config$workbook_format

  font_ref <- .style_font(workbook_format)

  # Create a table_name from tab_title (unique, no spaces, no punctuation)
  aftable[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable[["tab_title"]])))
  aftable[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable[["table_name"]], perl = TRUE)

  # Create workbook, set base style, set properties, add tabs, cover, contents (required for all workbooks)
  wb <- wb_workbook(theme = "Office 2007 - 2010 Theme")
  wb <- .set_workbook_properties(wb, workbook_properties)
  wb <- .style_workbook(wb, font_ref)
  wb <- .add_tabs(wb, aftable)
  wb <- .add_cover(wb, aftable, font_ref)
  wb <- .add_contents(wb, aftable, font_ref, workbook_format)

  # There won't always be a notes tab
  if (any(aftable$sheet_type %in% "notes")) {
    wb <- .add_notes(wb, aftable, font_ref, workbook_format)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable[aftable$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- .add_tables(wb, aftable, table_name = i, font_ref, workbook_format)
  }

  wb
}
