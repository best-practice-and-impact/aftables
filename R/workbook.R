#' Generate A Workbook Object From An 'aftable'
#'
#' Populate an 'openxlsx2' wbWorkbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx2]{wb_save}} from 'openxlsx2'
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#' @param author optional character string containing author to add to workbook.
#' Default NULL.
#' @param title optional character string containing title to add to workbook.
#' Default NULL.
#' @param keywords optional character vector containing keywords to add to workbook.
#' Default NULL.
#' @param config_path optional character string containing path to config file.
#' Defaults to config.yaml file located in working directory.
#' @param config_name optional character string specifying which configuration to use.
#' Default NULL.
#'
#' @return An openxlsx2 wbWorkbook-class object.
#'
#' @details
#'
#' See \code{\link[aftables]{create_config_yaml}} for details of the config.yaml
#' file, including how to add and edit configurations.
#'
#' If author, title and/or keywords are provided in both the `config.yaml` file
#' and in the function arguments, the values provided in the function arguments
#' are preferred to those provided in the `config.yaml` file. If `config_name`
#' is provided, values set in the `config_name` configuration in the
#' `config.yaml` file are preferred over those provided in the `default`
#' configuration. Values which are missing from the `config_name` configuration
#' are taken from the `default` configuration.
#'
#' Analysis Function guidance advises workbooks should have the author, title,
#' keywords and language fields completed. aftables provides functionality to
#' set author, title and keywords fields for workbooks. See [Releasing statistics in spreadsheets](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/#section-15)
#' for more information including how to set language for workbooks.
#'
#' @examples
#' # Convert an aftable to an openxlsx2 wbWorkbook-class object
#' \dontrun{
#' x <- generate_workbook(demo_aftable)
#' class(x)}
#'
#' # As above, using a compliant data.frame and the base pipe
#' \dontrun{
#' y <- demo_df |>
#'   as_aftable() |>
#'   generate_workbook()}
#'
#' # Using config.yaml file to set workbook properties and edit text and cell formatting
#' \dontrun{
#' example_workbook <- generate_workbook(demo_aftable,
#'                                       config_path = system.file("ext-data",
#'                                                                 "config.yaml",
#'                                                                 package = "aftables"),
#'                                       config_name = "default")}
#'
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' \dontrun{
#' openxlsx2::wb_get_properties(example_workbook)}
#'
#' # Setting the minimum workbook properties as function arguments
#' \dontrun{
#' example_workbook <- generate_workbook(demo_aftable,
#'                                       author = "Example author",
#'                                       title = "example workbook",
#'                                       keywords = c("keyword1",
#'                                                    "keyword2",
#'                                                    "keyword3"))}
#'
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' \dontrun{
#' openxlsx2::wb_get_properties(example_workbook)}
#'
#' # Save the workbook with openxlsx2::wb_save
#' \dontrun{
#' openxlsx2::wb_save(example_workbook, "example_workbook.xlsx")}
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

  user_config <- list(
    workbook_properties = list(
      author = author,
      title = title,
      keywords = keywords
    ),
    workbook_format = list()
  )

  config <- process_config(user_config, config_path, config_name)

  workbook_properties <- config$workbook_properties
  workbook_format <- config$workbook_format

  # Create a table_name from tab_title (unique, no spaces, no punctuation)
  aftable[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable[["tab_title"]])))
  aftable[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable[["table_name"]], perl = TRUE)

  # Create workbook, set base style, set properties, add tabs, cover, contents (required for all workbooks)
  wb <- wb_workbook(theme = "Office 2007 - 2010 Theme")
  wb <- .set_workbook_properties(wb, workbook_properties)
  wb <- .style_workbook(wb, workbook_format)
  wb <- .add_tabs(wb, aftable)
  wb <- .add_cover(wb, aftable, workbook_format)
  wb <- .add_contents(wb, aftable, workbook_format)

  # There won't always be a notes tab
  if (any(aftable$sheet_type %in% "notes")) {
    wb <- .add_notes(wb, aftable, workbook_format)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable[aftable$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- .add_tables(wb, aftable, table_name = i, workbook_format)
  }

  wb
}
