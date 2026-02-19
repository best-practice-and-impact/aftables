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
    stop("Error copying config.yaml",
         .call = FALSE)
  }

  if (path == getwd()) {
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

# process config arguments
process_config <- function(config_options) {

  # remove null config options not set by user
  config_options <- config_options[!sapply(config_options, is.null)]

  config <- list(workbook_properties = NULL,
                 workbook_format = NULL)

  # default to no user config set
  user_config <- "none"

  if (file.exists(config_options$config_path)) {
    config_file <- read_yaml(config_options$config_path)

    aftables_config <- NULL
    default_config <- NULL
    custom_config <- NULL

    if (purrr::pluck_exists(config_file, "aftables")) {

      aftables_config <- purrr::pluck(config_file, "aftables")

      default_config <- purrr::pluck(aftables_config, "default")

      # only extract custom config if user provides it
      if (!is.null(config_options$config_name)) {
        custom_config <- purrr::pluck(aftables_config, config_options$config_name)
      }

      # user did not set any config options
      # try and process default_config
      if (config_options$config_path == "config.yaml" &&
            is.null(config_options$config_name)) {
        user_config <- "default"
      }

      # user set default config options
      # process default_config
      if (!is.null(config_options$config_name) &&
            config_options$config_name == "default") {

        if (is.null(default_config)) {
          stop("The default key doesn't exist in the config file. Please view the documentation for create_config_yaml for an example aftables config file.",
               .call = FALSE)
        }

        user_config <- "default"
      }

      # user set custom config options
      # process default_config and custom_config
      if (!is.null(config_options$config_name) &&
            config_options$config_name != "default") {

        # error if both default config and custom config are missing
        if (is.null(default_config) && is.null(custom_config)) {
          stop(
            paste0(
              "The default key and the ",
              config_options$config_name,
              " key don't exist in the config file. Please view the documentation for create_config_yaml for an example aftables config file."
            ),
            .call = FALSE
          )
        }

        user_config <- "custom"
      }
    }
  }

  # process default_config
  # if default_config is NULL
  if (user_config == "default") {
    config$workbook_properties <- default_config$workbook_properties
    config$workbook_format <- default_config$workbook_format
  }

  # process default_config and/or custom_config
  if (user_config == "custom") {

    # no default config, use custom config only
    if (is.null(default_config)) {
      config$workbook_properties <- custom_config$workbook_properties
      config$workbook_format <- custom_config$workbook_format
    }

    # merge configs, preferring custom config
    if (!is.null(default_config)) {
      merged_config <- wb_merge_configs(default_config, custom_config)

      config$workbook_properties <- merged_config$workbook_properties
      config$workbook_format <- merged_config$workbook_format
    }
  }

  # update workbook_properties with values from arguments if set by user
  config_arguments <- c("author", "title", "keywords")
  config_arguments <- config_arguments[config_arguments %in% names(config_options)]

  if (length(config_arguments) > 0) {
    # if no config file was processed use values set by user
    if (is.null(config$workbook_properties)) {
      config$workbook_properties <-
        config_options[config_arguments]
    } else {
      # replace the config file values with values set by user
      config$workbook_properties <-
        purrr::list_modify(config$workbook_properties,
                           !!!config_options[config_arguments])
    }
  }

  # validate the final config
  validate_config(config)

  output <- list(workbook_properties = config$workbook_properties,
                 workbook_format = config$workbook_format)

  output
}

# check config field datatypes
validate_config <- function(config) {

  workbook_properties <- pluck(config,
                               "workbook_properties")

  workbook_format <- pluck(config,
                           "workbook_format")

  config_datatypes <- c(
    lapply(workbook_properties, typeof),
    lapply(workbook_format, typeof)
  ) |>
    unlist()

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
    "keywords" = "character"
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

wb_merge_configs <- function(default_config, custom_config) {

  merged_config <- purrr::list_modify(default_config, !!!custom_config)

  merged_config$workbook_properties$keywords <-
    c(default_config$workbook_properties$keywords,
      custom_config$workbook_properties$keywords)

  merged_config
}
