#' Create an aftables config.yaml
#'
#' Copy the example config.yaml file included with aftables to a directory of
#' the user's choice, and optionally open the file for editing. The config.yaml
#' file can be passed to the aftables function
#' \code{\link[aftables]{generate_workbook}}.
#'
#' @param path optional character string containing directory to copy the
#'   config.yaml file. Defaults to current working directory.
#' @param open_config optional logical whether to open the copy of config.yaml
#'   for editing in the current R session.
#'
#' @details If there is an existing config.yaml file in the destination
#' directory this function will not overwrite it.
#'
#' Contents of example config.yaml:
#'
#' ```{r results='asis', echo = FALSE}
#' cat('```xml\n')
#' cat(readLines(system.file("ext-data",
#'                           "config.yaml",
#'                           package = "aftables")),
#'     sep = "\n")
#' cat('\n```\n')
#' ```
#'
#' All configurations must be placed below an `aftables` key. `aftables` should
#' be followed by a `default` key and/or custom keys (e.g. `workbook1`).
#'
#' The `default` key settings will be read by all calls to
#' \code{\link[aftables]{generate_workbook}} which use this config.yml. This
#' allows you to share settings when generating multiple workbooks in one
#' script.
#'
#' Custom key settings (e.g. `workbook1`) will only be used by
#' \code{\link[aftables]{generate_workbook}} when the key is provided as the
#' `config_name` argument. This allows you to specify settings for a specific
#' workbook. Custom key settings will be preferred over the `default` settings.
#'
#' Not all workbook configuration options need to be set. Required settings are
#' documented in \code{\link[aftables]{generate_workbook}}.
#'
#' @examples
#' # Use default arguments to copy `aftables` `config.yaml` file to the current
#' # working directory without opening the copied file for editing:
#'
#' \dontrun{
#' create_config_yaml(open_config = FALSE)}
#'
#' @export
create_config_yaml <- function(path = getwd(),
                               open_config = rlang::is_interactive()) {

  if (!file.exists(path)) {
    stop(
      paste0("The directory `", path, "` does not exist."),
      call. = FALSE
    )
  }

  if (file.exists(paste0(path, "/config.yaml"))) {
    stop(
      paste0("`", path, "/config.yaml` already exists."),
      call. = FALSE
    )
  }

  copy <- file.copy(
    from = system.file("ext-data", "config.yaml", package = "aftables"),
    to = path,
    overwrite = FALSE,
    copy.mode = FALSE
  )

  if (!copy) {
    stop("Error copying config.yaml")
  }

  if (missing(path)) {
    warning(
      paste0("config.yaml copied to working directory. The default options for generate_workbook will use this file."),
      call. = FALSE
    )
  } else {
    message(
      paste0("config.yaml copied to ", path, " folder.")
    )
  }

  if (open_config) {
    file.edit(paste0(path, "/config.yaml"))
  }

  invisible(NULL)

}

# check wb_config field datatypes
wb_config_check <- function(wb_config) {

  workbook_properties <- pluck(wb_config,
                               "workbook_properties")

  workbook_format <- pluck(wb_config,
                           "workbook_format")

  config_datatypes <- c(lapply(workbook_properties, typeof),
                        lapply(workbook_format, typeof)) |> unlist()

  # the config may contain any of these entries and they should be these datatypes
  # if the config contains any extra entries they won't be used by functions
  # the functions will handle non-existent or empty entries as NULL

  complete_datatypes <- c(
    "base_font_size" = "integer",
    "table_header_size" = "integer",
    "sheet_header_size" = "integer",
    "cellwidth_default" = "integer",
    "cellwidth_wider" = "integer",
    "nchar_break" = "integer",
    "author" = "character",
    "title" = "character",
    "category" = "character",
    "subject" = "character",
    "modifier" = "character",
    "comments" = "character",
    "base_font_name"  = "character",
    "keywords" = "list"
  )

  datatypes_to_check <- complete_datatypes[names(config_datatypes)]

  if (!all(datatypes_to_check == config_datatypes)) {

    invalid_data <- names(datatypes_to_check[datatypes_to_check != config_datatypes])

    stop(
      c(
        "Please review the following config.yaml entries: ",
        paste0(invalid_data, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)

}

wb_merge_configs <- function(default_config, user_config) {

  # get entries in default_config which are not in user config
  keys <- setdiff(names(default_config), names(user_config))

  merged_config <- default_config[keys]

  # append entries in user config which are not in default config
  keys <- setdiff(names(user_config), names(default_config))
  merged_config <- c(merged_config, user_config[keys])

  # get entries in both user_config and default_config
  keys <- intersect(names(user_config), names(default_config))

  # append user_config list items to default_config list items (keywords)
  # and replace default_config items with user_config items
  merged_config <- c(
    merged_config,
    ifelse(
      sapply(default_config[keys], typeof) == "list",
      stats::setNames(mapply(c,
                             default_config[keys],
                             user_config[keys]),
                      keys),
      default_config[keys] <- user_config[keys]
    )
  )

  merged_config
}
