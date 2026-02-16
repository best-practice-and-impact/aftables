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
#' @param config_path optional character string containing directory where config yaml
#' file is stored. Defaults to working directory.
#' @param config_file optional character string containing name of config yaml file.
#' Default `config.yaml`.
#' @param config_name optional character string specifying which configuration to use.
#' Default `default`
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
                              config_path = ".",
                              config_file = "config.yaml",
                              config_name = "default") {

  if (!is_aftable(aftable)) {
    stop("The object passed to argument 'content' must have class 'aftable'.")
  }

  #  if no path provided look in working directory
  if (is.null(config_path) || missing(config_path)) {
    config_location <- paste0("./", config_file)
  } else {
    config_location <- paste0(config_path, "/", config_file)
  }

  # user provided path to a config.yaml file that doesn't exist
  if (!missing(config_path) && !file.exists(paste0(config_location))) {
    stop(
      paste0(
        "config_path set but config file not found. Please check that '",
        config_file,
        "' exists in",
        config_path,
        " directory."
      ),
      call. = FALSE
    )
  }

  wb_config <- NULL

  if (file.exists(paste0(config_location))) {
    # config file found in working directory
    wb_config <- read_yaml(file = config_location)

    # check if aftables key exists in config
    if (is.null(pluck(wb_config, "aftables"))) {
      stop(
        paste0(
          "aftables key does not exist in ",
          config_file,
          ". Please check there is an aftables key in your ",
          config_file,
          ", file."
        ),
        call. = FALSE
      )
    }

    # warn if default does not exist
    if (is.null(pluck(wb_config, "aftables", "default"))) {
      warning(
        paste0(
          "default config does not exist in ",
          config_file,
          ". Please check there is a default key in your ",
          config_file,
          " file."
        ),
        call. = FALSE
      )
    }

    # error if user set key does not exist
    if (config_name != "default" && is.null(pluck(wb_config, "aftables", config_name))) {
      warning(
        paste0(
          "user set config does not exist in ",
          config_file,
          ". Please check there is a ",
          config_name,
          " key in your ",
          config_file,
          " file."
        ),
        call. = FALSE
      )
    }

    wb_config_default <- pluck(wb_config, "aftables", "default")

    wb_config_user <- pluck(wb_config, "aftables", config_name)

    # combine wb_config that user requested with default config, preferring user config if values set
    wb_config_combined <- list()

    if (!is.null(wb_config_user$workbook_properties) && config_name != "default") {
      wb_config_combined$workbook_properties <- wb_merge_configs(wb_config_default$workbook_properties,
                                                                 wb_config_user$workbook_properties)
    } else {
      wb_config_combined$workbook_properties <- wb_config_default$workbook_properties
    }

    if (!is.null(wb_config_user$workbook_format) && config_name != "default") {
      wb_config_combined$workbook_format <- wb_merge_configs(wb_config_default$workbook_format,
                                                             wb_config_user$workbook_format)
    } else {
      wb_config_combined$workbook_format <- wb_config_default$workbook_format
    }

    # validate config data types
    wb_config_check(wb_config_combined)

    workbook_format <- wb_config_combined$workbook_format

    workbook_properties <-
      c(
        wb_config_combined$workbook_properties,
        list(
          datetime_created = Sys.time(),
          datetime_modified = Sys.time()
        )
      )

  } else { # otherwise no config provided create minimum workbook_properties

    workbook_properties <- vector("list", 5)

    names(workbook_properties) <- c("author",
                                    "title",
                                    "keywords",
                                    "datetime_created",
                                    "datetime_modified")

    workbook_properties$datetime_created <- Sys.time()
    workbook_properties$datetime_modified <- Sys.time()

    workbook_format <- NULL

  }

  # if arguments set then process arguments

  # else no config

  # if user does set config path and doesn't set config name
  # file has aftables config then process it
  # file does not have aftables config then error

  # user does not set config path and does set config name
  # file does not exist then error
  # file exists but config doesn't then error
  # file exists and config exists then process

  # user sets config path and config name
  # file does not exist then error
  # file exists but config doesn't then error
  # file exists and config exists then process

  # process config:
  # check config file
  # if config file contains any of the workbook_parameters or workbook_formats
  # entries then check if those datatypes are correct

  # if pass then process config
  # if arguments then process arguments

  # use workbook_properties from arguments if config is missing/empty
  if (is.null(workbook_properties$author)) {
    workbook_properties$author <- as.character(author)
  }

  if (is.null(workbook_properties$title)) {
    workbook_properties$title <- as.character(title)
  }

  if (is.null(workbook_properties$keywords)) {
    workbook_properties$keywords <- keywords
  }

  if (any(is.null(workbook_properties$author),
          is.null(workbook_properties$title),
          is.null(workbook_properties$keywords))) {
    warning(
      "Minimum workbook properties have not been set. Analysis Function guidance recommends at a minimum setting workbook author, title and keywords/tags",
      call. = FALSE
    )
  }

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
