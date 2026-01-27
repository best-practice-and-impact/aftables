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
#' function will not overwrite it. If open_config is set to TRUE in this
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
#' It is recommended to use the default configuration as a template for a new
#' entry. Copy the default entry, paste it below the minimum entry and replace
#' `default` with a new name. The name of an entry is used in the `config_name`
#' argument in the aftables `generate_workbook` function.
#'
#' The aftables `generate_workbook` function requires config.yaml to include a
#' default configuration, and will result in an error if it is removed.
#'
#' @examples
#'
#' # Copy default aftables config.yaml file to user's home directory, and open
#' # the copied file for editing.
#' \dontrun{
#' get_config_yaml(path = "~", open_config = TRUE)
#' }
#'
#' @export
get_config_yaml <- function(path = NULL, open_config = FALSE) {

  if (is.null(path)) path <- getwd()

  if (file.exists(path)) {
    file.copy(from = system.file("ext-data",
                                 "config.yaml",
                                 package = "aftables"),
              to = path,
              overwrite = FALSE,
              copy.mode = FALSE)

    if (path == "~") {
      print(paste0("config.yaml copied to user's home directory."))
    } else {
      print(paste0("config.yaml copied to ", path, " folder."))
    }

    if (open_config) {
      file.edit(paste0(path, "/config.yaml"))
    }
  } else {
    stop(paste0("The directory `", path, "` does not exist."))
  }

  invisible(NULL)

}
