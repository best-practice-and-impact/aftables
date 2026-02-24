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


# Process config arguments
process_config <- function(user_config, config_path, config_name) {

  # Get settings from config file if exists ------------------------------------

  if (!is.null(config_path) && config_path != "config.yaml" &&
        !file.exists(config_path)) {
    stop("Config file ", config_path, " does not exist", call. = FALSE)
  }

  if ((is.null(config_path) || !file.exists(config_path)) &&
        !is.null(config_name)) {
    stop(
      "config_name has been set but config file does not exist",
      call. = FALSE
    )
  }

  if (!is.null(config_path) && file.exists(config_path)) {

    config_file <- read_yaml(config_path)

    if (config_path != "config.yaml" &&
          !purrr::pluck_exists(config_file, "aftables")) {
      stop(
        "Config file ", config_path, " does not contain an aftables key",
        call. = FALSE
      )
    }

    if (config_path == "config.yaml" &&
          !purrr::pluck_exists(config_file, "aftables")) {
      warning(
        "Config file ", config_path,
        " does not contain an aftables key and will therefore be ignored",
        call. = FALSE
      )
    }


    # Get default config settings ---------

    if (is.null(config_name) &&
          purrr::pluck_exists(config_file, "aftables") &&
          !purrr::pluck_exists(config_file, "aftables", "default")) {
      stop(
        "Config file ", config_path,
        " does not contain a default aftables configuration and a custom key is not being used",
        call. = FALSE
      )
    }

    default_config <- purrr::pluck(
      config_file,
      "aftables", "default",
      .default = list()
    )

    if (!is.list(default_config)) {
      stop("Default configuration key must be a named list", call. = FALSE)
    }


    # Get custom config settings ---------

    if (!is.null(config_name)) {

      if (!purrr::pluck_exists(config_file, "aftables", config_name)) {
        stop(
          "Config file ", config_path, " does not contain key `",
          config_name, "`",
          call. = FALSE
        )
      }

      custom_config <- purrr::pluck(
        config_file,
        "aftables", config_name,
        .default = list()
      )

      if (!is.list(custom_config)) {
        stop(
          "Configuration key ", config_name, " must be a named list",
          call. = FALSE
        )
      }

    } else {
      custom_config <- list()
    }


  } else {
    # No config file
    default_config <- list()
    custom_config <- list()
  }


  # Combine config options -----------------------------------------------------
  # Remove null options in user config - these are default function argument
  user_config <- purrr::map_depth(user_config, 1, plyr::compact)

  # Combine config settings, user config has highest priority, default config lowest
  config <-  purrr::list_modify(default_config, !!!custom_config)
  config <-  purrr::list_modify(config, !!!user_config)

  # convert list of keywords to vector
  config$workbook_properties$keywords <-
    config$workbook_properties$keywords |>
    unlist(use.names = FALSE)


  # Validate the final config --------------------------------------------------
  validate_config(config)

  config
}



# check config field datatypes
validate_config <- function(config) {

  # remove any NULLs from config before processing
  config <- purrr::map_depth(config, 1, plyr::compact)

  # order any fields alphabetically
  config <- purrr::map_depth(config, 1, ~ if (length(.x) > 0) .x[order(names(.x))])

  config <- purrr::map_depth(config, 1, ~ switch(typeof(.x),
                                                 "list" = .x[order(names(.x))],
                                                 "NULL" = list()))

  # get datatypes from config
  config_datatypes <- purrr::map_depth(config, 2, typeof)

  # the config may contain any of these entries and they should be these datatypes
  # if the config contains any extra entries they won't be used by functions
  # the functions will handle non-existent or empty entries as NULL

  correct_datatypes <- list(
    "workbook_properties" =
      list(
        "author" = "character",
        "category" = "character",
        "comments" = "character",
        "company" = "character",
        "keywords" = "character",
        "manager" = "character",
        "modifier" = "character",
        "subject" = "character",
        "title" = "character"
      ),
    "workbook_format" =
      list(
        "base_font_name"  = "character",
        "base_font_size" = "integer",
        "cellwidth_default" = "integer",
        "cellwidth_wider" = "integer",
        "nchar_break" = "integer",
        "sheet_header_size" = "integer",
        "table_header_size" = "integer"
      )
  )

  config_names <- purrr::map_depth(config_datatypes, 1, names)
  correct_names <- purrr::map_depth(correct_datatypes, 1, names)

  # determine which config_fields to check
  correct_fields <- purrr::map2(correct_names,
                                config_names,
                                \(x, y) x %in% y)

  config_fields <- purrr::map2(config_names,
                               correct_names,
                               \(x, y) x %in% y)

  # filter both complete_datatypes and config_datatypes to match on entries
  correct_datatypes <-
    purrr::map2(correct_datatypes,
                correct_fields,
                \(x, y) x[y])

  config_datatypes <-
    purrr::map2(config_datatypes,
                config_fields,
                \(x, y) x[y])

  # remove any NULLs introduced
  correct_datatypes <- purrr::map_depth(correct_datatypes, 1, plyr::compact)
  config_datatypes <- purrr::map_depth(config_datatypes, 1, plyr::compact)

  # use all.equal to compare user config_datatypes to correct_datatypes
  invalid_config_entries <-
    all.equal(config_datatypes,
              correct_datatypes,
              check.class = FALSE)

  if (!isTRUE(invalid_config_entries)) {

    # clean output of all.equal
    invalid_config_entries <-
      invalid_config_entries |>
      stringr::str_remove_all(pattern = "[\u201C|\u201D]") |> # “”
      stringr::str_extract_all(pattern = "(?<=Component[[:space:]])\\b\\w*\\b")

    # get correct data types for invalid entries
    valid_datatypes <-
      purrr::map(invalid_config_entries,
                 \(x) purrr::pluck(correct_datatypes, x[1], x[2]))

    # generate error messages for user
    invalid_config_entries <-
      mapply(c,
             invalid_config_entries,
             valid_datatypes,
             SIMPLIFY = FALSE)

    invalid_config_entries <-
      lapply(invalid_config_entries,
             str_replace_all,
             pattern = "character",
             replacement = "character string")

    invalid_config_entries <-
      lapply(invalid_config_entries,
             str_replace_all,
             pattern = "numeric",
             replacement = "numeric value")

    invalid_config_entries <-
      invalid_config_entries |>
      sapply(paste0,
             sep = c(": ", " should be ", ""),
             collapse = "")

    stop(
      c(
        "Please review the following invalid config entries:\n",
        paste0(invalid_config_entries, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)

}
