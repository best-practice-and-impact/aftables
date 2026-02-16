#' Function to export  file included with aftables
#'
#' Copy the example config.yaml file included with aftables to a directory of
#' the users choice, and optionally open the file for editing. The config.yaml
#' file can be passed to the aftables function
#' \code{\link[aftables]{generate_workbook}}.
#'
#' @param path optional character string containing directory to copy the
#' config.yaml file. Defaults to current working directory.
#' @param open_config optional logical whether to open the copy of config.yaml
#' for editing in the current R session. Default FALSE.
#'
#' @details
#' If there is an existing config.yaml file in the destination directory this
#' function will not overwrite it. If `open_config` is set to TRUE in this
#' situation the existing config.yaml file will be opened.
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
#' All configurations must be placed below an `aftables` key. The `aftables`
#' config yaml file can be combined with other config yaml files as long as the
#' `aftables` key is preserved so the \code{\link[aftables]{generate_workbook}}
#' can find and process the `aftables` configurations.
#'
#' It is recommended to use the `default` configuration as a template. Edit the
#' `default` configuration and optionally replace `default` with a new key. New
#' configurations can be appended below the `default` key. The configuration key
#'  is used in the `config_name` argument in the \code{\link[aftables]{generate_workbook}} function.
#'
#' Values can be set in the `default` configuration and any user-created
#' configurations. When a non-default configuration key is provided as the
#' `config_name` argument to \code{\link[aftables]{generate_workbook}}, the
#' values set in the non-default configuration will be preferred over the
#' `default` configuration, even if they are set in the `default` configuration.
#' If there is no `default` configuration all values will be taken from the
#' non-default configuration.
#'
#' @examples
#' # Use default arguments to copy `aftables` `config.yaml` file to the current
#' # working directory without opening the copied file for editing:
#'
#' \dontrun{
#' create_config_yaml()}
#'
#' # Copy aftables config.yaml file to user's home directory, and open
#' # the copied file for editing:
#'
#' \dontrun{
#' create_config_yaml(path = "~", open_config = TRUE)}
#'
#' @export
create_config_yaml <- function(path = getwd(), open_config = FALSE) {

  if (file.exists(path)) {
    file.copy(from = system.file("ext-data",
                                 "config.yaml",
                                 package = "aftables"),
              to = path,
              overwrite = FALSE,
              copy.mode = FALSE)

    if (missing(path)) {
      warning(
        paste0("config.yaml copied to working directory. The default options for generate_workbook will use this file."),
        call. = FALSE
      )
    } else {
      message(
        paste0("config.yaml copied to ", path, " folder.",
               call. = FALSE)
      )
    }

    if (open_config) {
      file.edit(paste0(path, "/config.yaml"))
    }
  } else {
    stop(
      paste0("The directory `", path, "` does not exist."),
      call. = FALSE
    )
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
