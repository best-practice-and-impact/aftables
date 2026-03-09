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
#' Keys below workbook_properties will appear in the Excel workbook when it is
#' saved using \code{\link[openxlsx2]{wb_save}} from openxlsx2. They can be
#' found in the file properties or the workbook information.
#'
#' Keys below workbook_format will be applied to the contents of the workbook.
#' The keys base_font_name and base_font_size define the default font name and
#' size used by the workbook. All text not formatted as a table header, sheet
#' subheading or sheet heading will use the default settings. Font sizes of
#' sheet headings, sheet subheadings, and table header rows can be set using the
#' sheet_heading_size, sheet_subheading_size and table_header_size keys
#' respectively, and they will additionally be formatted as bold.
#'
#' The keys cellwidth_default, cellwidth_wider and nchar_break are used to
#' define column widths. The units of the cellwidth_default and cellwidth_wider
#' keys are the column width values used by Excel. All columns widths are set by
#' default to use the cellwidth_default value. If the number of characters in a
#' column header or the contents of a column exceeds the value of nchar_break
#' aftables will set the column width to the value of cellwidth_wider. Users can
#' avoid text wrapping in columns or column headers by setting the value of
#' nchar_break based on their data or the content of their column headers.
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
         call. = FALSE)
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
    utils::file.edit(paste0(path, "/config.yaml"))
  }

  invisible(NULL)

}


# Process config arguments
.process_config <- function(user_config, config_path, config_name) {

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

    config_file <- yaml::read_yaml(config_path)

    if (config_path != "config.yaml" &&
          !pluck_exists(config_file, "aftables")) {
      stop(
        "Config file ", config_path, " does not contain an aftables key",
        call. = FALSE
      )
    }

    if (config_path == "config.yaml" &&
          !pluck_exists(config_file, "aftables")) {
      warning(
        "Config file ", config_path,
        " does not contain an aftables key and will therefore be ignored",
        call. = FALSE
      )
    }


    # Get default config settings ---------

    if (is.null(config_name) &&
          pluck_exists(config_file, "aftables") &&
          !pluck_exists(config_file, "aftables", "default")) {
      stop(
        "Config file ", config_path,
        " does not contain a default aftables configuration and a custom key is not being used",
        call. = FALSE
      )
    }

    if (pluck_exists(config_file, "aftables", "default")) {

      default_config <- pluck(config_file, "aftables", "default")

      if (!is.list(default_config) ||
        length(default_config) > 2 ||
        is.null(names(default_config)) ||
        !all(
          names(default_config) %in% c("workbook_properties", "workbook_format")
        )) {
        stop(
          "Default configuration key must be a named list. ",
          "It can only contain keys `workbook_properties` and `workbook_format`.",
          call. = FALSE
        )
      }

      if (pluck_exists(default_config, "workbook_properties") &&
            (!is.list(default_config$workbook_properties) ||
               is.null(names(default_config$workbook_properties)))) {
        stop(
          "Default configuration workbook_properties must be a named list",
          call. = FALSE
        )
      }

      if (pluck_exists(default_config, "workbook_format") &&
            (!is.list(default_config$workbook_format) ||
               is.null(names(default_config$workbook_format)))) {
        stop(
          "Default configuration workbook_format must be a named list",
          call. = FALSE
        )
      }

    } else {
      # No default config
      default_config <- list()
    }



    # Get custom config settings ---------

    if (!is.null(config_name)) {

      if (!pluck_exists(config_file, "aftables", config_name)) {
        stop(
          "Config file ", config_path, " does not contain key `",
          config_name, "`",
          call. = FALSE
        )
      }

      custom_config <- pluck(
        config_file,
        "aftables", config_name,
        .default = list()
      )

      if (!is.list(custom_config)  ||
        length(custom_config) > 2 ||
        is.null(names(custom_config)) ||
        !all(
          names(custom_config) %in% c("workbook_properties", "workbook_format")
        )) {
        stop(
          "Configuration key ", config_name, " must be a named list. ",
          "It can only contain keys `workbook_properties` and `workbook_format`.",
          call. = FALSE
        )
      }

      if (!is.null(custom_config$workbook_properties) &&
            (!is.list(custom_config$workbook_properties) ||
               is.null(names(custom_config$workbook_properties)))) {
        stop(
          "Custom configuration workbook_properties must be a named list",
          call. = FALSE
        )
      }

      if (!is.null(custom_config$workbook_format) &&
            (!is.list(custom_config$workbook_format) ||
               is.null(names(custom_config$workbook_format)))) {
        stop(
          "Custom configuration workbook_format must be a named list",
          call. = FALSE
        )
      }

    } else {
      # No custom config
      custom_config <- list()
    }


  } else {
    # No config file
    default_config <- list()
    custom_config <- list()
  }


  # Combine config options -----------------------------------------------------
  # Remove null options in user config - these are default function argument
  user_config <- map_depth(user_config, 1, compact)

  # Combine config settings, user config has highest priority, default config lowest
  config <-  list_modify(default_config, !!!custom_config)
  config <-  list_modify(config, !!!user_config)

  # Warning if the config workbook_properties have not been
  # changed from the internal config.yaml defaults
  if (length(config$workbook_properties) > 0 &&
      any(unlist(config$workbook_properties,
                 use.names = FALSE) %in%
          c("aftables",
            "aftables example workbook",
            "aftables workbook 2",
            "aftables1",
            "aftables2",
            "aftables3",
            "aftables example subject",
            "aftables example category",
            "aftables workbook 1 category",
            "aftablesworkbook1",
            "aftables workbook 2 category",
            "aftablesworkbook2",
            "aftables example comments")
      )
  ) {
    warning(
      paste0(
        "Your config file contains values identical to the aftables example ",
        "config. Please check your config file."
      ),
      call. = FALSE
    )
  }

  # Validate the final config --------------------------------------------------
  .validate_config(config)


  # warning if any config provided and any of recommended properties author,
  # title or keywords are missing from config
  if (!all(c(pluck_exists(config, "workbook_properties", "author"),
             pluck_exists(config, "workbook_properties", "keywords"),
             pluck_exists(config, "workbook_properties", "title")))) {
    warning(paste0("Some of the recommended workbook properties are missing. ",
                   "Analysis Function guidance recommends completing the ",
                   "author, title and keywords fields."),
            call. = FALSE)
  }

  config
}

# check config field datatypes
.validate_config <- function(config) {

  # The config may contain any of these entries and they should be these
  # datatypes. If the config contains any extra entries they won't be used by
  # functions. Functions will handle non-existent or empty entries as NULL.

  correct_datatypes <-
    tibble::tibble(
      correct_parent = c(
        rep("workbook_properties", 6),
        rep("workbook_format", 8)
      ),
      entry = c(
        "author",
        "category",
        "comments",
        "keywords",
        "subject",
        "title",
        "base_font_name",
        "base_font_size",
        "cellwidth_default",
        "cellwidth_wider",
        "nchar_break",
        "sheet_heading_size",
        "sheet_subheading_size",
        "table_header_size"
      ),
      datatype = c(
        rep("character", 7),
        rep("integer", 7)
      )
    )

  # Turn config list into dataframe
  config_df <-
    tibble::tibble(
      # create vector of workbook_properties and workbook_format
      # with length equal to number of entries under each
      parent = rep(map_depth(config, 0, names) |>
                     unlist(use.names = FALSE) %||% character(0),
                   map_depth(config, 1, length) |>
                     unlist(use.names = FALSE) %||% 0),
      # get names of each entry under workbook_properties and workbook_format
      entry =  map_depth(config, 1, names) |>
        unlist(use.names = FALSE) %||% character(0),
      # Find the datatype of each entry under workbook_properties and
      # workbook_format
      # Replace NULL with character(0) so column is always created
      config_datatype = map_depth(config, 2, typeof) |>
        unlist(use.names = FALSE) %||% character(0),
      # Find the length of each entry under workbook_properties and
      # workbook_format
      # Replace NULL with character(0) so column is always created
      config_length = map_depth(config, 2, length) |>
        unlist(use.names = FALSE) %||% character(0)
    )

  # Warning if entries in user config are not recognised
  # either wrong name or under the wrong entry (workbook_properties/workbook_format)
  unrecognised_entries <-
    dplyr::anti_join(
      config_df,
      correct_datatypes,
      by = dplyr::join_by("parent" == "correct_parent", "entry")
    ) |>
    mutate(message = paste0(.data$parent, ":", .data$entry))

  if (nrow(unrecognised_entries) > 0) {
    warning(
      paste0(
        "Some entries in your config file could not be processed.\n",
        "Entries with invalid names or in the wrong location will ",
        "not be added to your workbook.\n",
        "Please check the names and locations of these entries ",
        "in your config file:\n",
        paste0(unrecognised_entries$message, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # Combine the configs
  combined_configs <-
    dplyr::inner_join(
      correct_datatypes,
      config_df,
      by = dplyr::join_by("correct_parent" == "parent", "entry")
    )

  # Keywords can be any length, all other entries should be 1
  config_lengths <-
    combined_configs |>
    filter(.data$entry != "keywords" & .data$config_length > 1)

  # Error if any entries contain more than 1 value apart from keywords
  if (nrow(config_lengths) > 0) {
    stop(
      paste0(
        "Config entries must contain only one value apart from keywords. ",
        "Please check your config file for these entries: ",
        paste0(config_lengths$entry, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # Identify entries with incorrect datatypes and create error messages
  incorrect_datatypes <-
    combined_configs |>
    filter(.data$datatype != .data$config_datatype) |>
    mutate(
      across(
        c("datatype", "config_datatype"),
        \(x) str_replace(x, "character", "character string")
      ),
      across(
        c("datatype", "config_datatype"),
        \(x) str_replace(x, "integer", "integer value")
      ),
      error_message = paste0(.data$correct_parent, ":", .data$entry, " is ",
                             .data$config_datatype, ". It should be ",
                             .data$datatype, ".")
    )

  # Error if any invalid config datatypes
  if (nrow(incorrect_datatypes) > 0) {
    stop(
      c(
        "Please review the following invalid config entries:\n",
        paste0(incorrect_datatypes$error_message, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)

}
