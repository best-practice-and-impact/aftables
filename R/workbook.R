#' Generate A Workbook Object From An 'aftable'
#'
#' Populate an 'openxlsx2' wbWorkbook-class object with content from an
#' aftable-class object. In turn, the output can be passed to
#' \code{\link[openxlsx2]{wb_save}} from 'openxlsx2'
#'
#' @param aftable An aftable-class object created using
#'     \code{\link{create_aftable}} (or \code{\link{as_aftable}}), which
#'     contains the data and information needed to create a workbook.
#' @param creator character string containing creator to add to workbook.
#' Default NULL.
#' @param title character string containing title to add to workbook.
#' Default NULL.
#' @param keywords character vector containing keywords to add to workbook.
#' Default NULL.
#' @param config_path character string containing path to config.yaml.
#' Default NULL.
#' @param config_name character string specifying which configuration to use.
#' Default NULL.
#'
#' @return An openxlsx2 wbWorkbook-class object.
#'
#' @details
#'
#' See \code{\link[aftables]{get_config_yaml}} for details of the config.yaml
#' file, including how to add and edit configurations.
#'
#' Analysis Function guidance advises workbooks should have the author, title,
#' keywords and language fields completed. aftables provides functionality to
#' set author, title and keywords fields for workbooks. See [Releasing statistics in spreadsheets](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/#section-15)
#' for more information including how to set language for workbooks.
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
#' # Using config.yaml file to set workbook properties and edit text and cell formatting
#' example_workbook <- generate_workbook(demo_aftable,
#'                                       config_path = system.file("ext-data",
#'                                                                 "config.yaml",
#'                                                                 package = "aftables"),
#'                                       config_name = "default")
#
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' openxlsx2::wb_get_properties(example_workbook)
#'
#' # Using config.yaml file to set minimum workbook properties and edit text and cell formatting
#' example_workbook <- generate_workbook(demo_aftable,
#'                                       config_path = system.file("ext-data",
#'                                                                 "config.yaml",
#'                                                                 package = "aftables"),
#'                                       config_name = "minimum")
#
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' openxlsx2::wb_get_properties(example_workbook)
#'
#' # Setting the minimum workbook properties without using a config.yaml file
#' example_workbook <- generate_workbook(demo_aftable,
#'                                       creator = "Example author",
#'                                       title = "example workbook",
#'                                       keywords = c("keyword1",
#'                                                    "keyword2",
#'                                                    "keyword3"))
#'
#' # Use openxlsx2::wb_get_properties to view properties that have been applied
#' openxlsx2::wb_get_properties(example_workbook)
#'
#' # Save the workbook with openxlsx2::wb_save
#' \dontrun{
#' openxlsx2::wb_save(example_workbook, "example_workbook.xlsx")}
#' @export
generate_workbook <- function(aftable, creator = NULL, title = NULL,
                              keywords = NULL, config_path = NULL,
                              config_name = NULL) {


  if (!is_aftable(aftable)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  # get parameters from config.yaml
  if (!is.null(config_path) && !is.null(config_name)) {
    wb_config <- read_yaml(file = config_path)
    # fetch.config gets the entire file, need to get only the user's desired config
    wb_config <- wb_config[[config_name]]

    parameters <- list(
      creator = wb_config$workbook_properties$creator,
      title =  wb_config$workbook_properties$title,
      subject =  wb_config$workbook_properties$subject,
      category = wb_config$workbook_properties$category,
      datetime_created = Sys.time(),
      datetime_modified = Sys.time(),
      modifier = wb_config$workbook_properties$modifier,
      keywords = paste(wb_config$workbook_properties$keywords, collapse = ", "),
      comments = wb_config$workbook_properties$comments,
      manager = wb_config$workbook_properties$manager,
      company = wb_config$workbook_properties$company,
      custom = wb_config$workbook_properties$custom
    )
  } else {
    wb_config <- NULL
    parameters <- list()

    if (!is.null(creator)) {
      parameters$creator <- creator
    }

    if (!is.null(title)) {
      parameters$title <- title
    }

    if (!is.null(keywords)) {
      parameters$keywords <- paste(keywords, collapse = ", ")
    }
  }

  # Create a table_name from tab_title (unique, no spaces, no punctuation)
  aftable[["table_name"]] <-
    gsub(" ", "_", tolower(trimws(aftable[["tab_title"]])))
  aftable[["table_name"]] <-
    gsub("(?!_)[[:punct:]]", "", aftable[["table_name"]], perl = TRUE)

  # Create workbook, set base style, add tabs, cover, contents (required for all workbooks)
  wb <- wb_workbook(theme = "Office 2007 - 2010 Theme")
  wb <- .set_workbook_parameters(wb, parameters)
  wb <- .style_workbook(wb, wb_config)
  wb <- .add_tabs(wb, aftable)
  wb <- .add_cover(wb, aftable, wb_config)
  wb <- .add_contents(wb, aftable, wb_config)

  # There won't always be a notes tab
  if (any(aftable$sheet_type %in% "notes")) {
    wb <- .add_notes(wb, aftable, wb_config)
  }

  # Iterable titles for tabs containing tables
  table_sheets <- aftable[aftable$sheet_type == "tables", ][["table_name"]]

  for (i in table_sheets) {
    wb <- .add_tables(wb, aftable, table_name = i, wb_config = wb_config)
  }

  wb
}
