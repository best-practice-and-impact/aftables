#' A Demo 'data.frame' Object
#'
#' A pre-created data.frame ready to be converted to an a11ytables-class object
#' with \code{\link{as_a11ytable}} and then an 'openxlsx' Workbook-class object
#' with \code{\link{generate_workbook}}.
#'
#' @format A data.frame with 6 rows and 7 columns:
#' \describe{
#'   \item{tab_title}{Character. Text to appear on each sheet's tab.}
#'   \item{sheet_type}{Character. The content type for each sheet: 'cover', 'contents', 'notes', or 'tables'.}
#'   \item{sheet_title}{Character. The title that will appear in cell A1 (top-left) of each sheet.}
#'   \item{blank_cells}{Character. An explanation for any blank cells in the table.}
#'   \item{custom_rows}{List-column of character vectors. Additional arbitrary pre-table information provided by the user.}
#'   \item{source}{Character. The origin of the data, if relevant.}
#'   \item{table}{List-column of data.frames (apart from the cover, which is a list) containing the statistical tables.}
#' }
"demo_df"

#' A Demo 'a11ytables' Object
#'
#' A pre-created 'a11ytables' object ready to be converted to an 'openxlsx'
#' Workbook-class object with \code{\link{generate_workbook}}.
#'
#' @format A data.frame with 6 rows and 7 columns:
#' \describe{
#'   \item{tab_title}{Character. Text to appear on each sheet's tab.}
#'   \item{sheet_type}{Character. The content type for each sheet: 'cover', 'contents', 'notes', or 'tables'.}
#'   \item{sheet_title}{Character. The title that will appear in cell A1 (top-left) of each sheet.}
#'   \item{blank_cells}{Character. An explanation for any blank cells in the table.}
#'   \item{custom_rows}{List-column of character vectors. Additional arbitrary pre-table information provided by the user.}
#'   \item{source}{Character. The origin of the data, if relevant.}
#'   \item{table}{List-column of data.frames (apart from the cover, which is a list) containing the statistical tables.}
#' }
"demo_a11ytable"

#' A Demo 'Workbook' Object
#'
#' A pre-created 'openxlsx' Workbook'-class object generated from an
#' a11ytables-class object with \code{\link{generate_workbook}}.
#'
#' @format An 'openxlsx' Workbook-class object with 5 sheets.
"demo_workbook"

#' Test Data: A Modified 'mtcars' Dataframe (Version 1)
#'
#' @description
#' Superseded. mtcars_df and \code{\link{mtcars_df2}} have been superseded in
#' favour of \code{\link{demo_df}}.
#'
#' A modified version of the mtcars dataset prepared into a data.frame structure
#' ready for coercion to an a11ytables-class object with
#' \code{\link{as_a11ytable}}. Uses a dataframe as input to the cover table;
#' \code{\link{mtcars_df}} uses a list as input to the cover table.
#'
#' @details
#' Uses a data.frame as input to the cover table, whereas
#' \code{\link{mtcars_df2}} uses a list as input to the cover table
#' (implemented in version 0.2).
#'
#' Note that this dataset is superseded by \code{\link{demo_df}} but is
#' retained for backwards-compatibility with package versions prior to 0.3.
#'
#' @format A data frame with 5 rows and 6 columns:
#' \describe{
#'   \item{tab_title}{Character. Text to appear on each sheet's tab.}
#'   \item{sheet_type}{Character. The content type for each sheet: 'cover', 'contents', 'notes', or 'tables'.}
#'   \item{sheet_title}{Character. The title that will appear in cell A1 (top-left) of each sheet.}
#'   \item{blank_cells}{Character. An explanation for any blank cells in the table.}
#'   \item{source}{Character. The origin of the data, if relevant.}
#'   \item{table}{List-column of data.frames containing the statistical tables.}
#' }
#'
#' @source \code{\link[datasets:mtcars]{mtcars}}
"mtcars_df"

#' Test Data: A Modified 'mtcars' Dataframe  (Version 2)
#'
#' @description
#' Superseded. \code{\link{mtcars_df}} and mtcars_df2 have been superseded in
#' favour of \code{\link{demo_df}}.
#'
#' A modified version of the mtcars dataset prepared into a data.frame structure
#' ready for coercion to an a11ytables-class object with
#' \code{\link{as_a11ytable}}.
#'
#' @details
#' Uses a list as input to the cover table (implemented in version 0.2), whereas
#' \code{\link{mtcars_df}} uses a data.frame as input to the cover table.
#'
#' Note that this dataset is superseded by \code{\link{demo_df}} but is
#' retained for backwards-compatibility with package versions starting 0.2.
#'
#' @format A data frame with 5 rows and 6 columns:
#' \describe{
#'   \item{tab_title}{Character. Text to appear on each sheet's tab.}
#'   \item{sheet_type}{Character. The content type for each sheet: 'cover', 'contents', 'notes', or 'tables'.}
#'   \item{sheet_title}{Character. The title that will appear in cell A1 (top-left) of each sheet.}
#'   \item{blank_cells}{Character. An explanation for any blank cells in the table.}
#'   \item{source}{Character. The origin of the data, if relevant.}
#'   \item{table}{List-column of data.frames (apart from the cover, which is a list) containing the statistical tables.}
#' }
#'
#' @source \code{\link[datasets:mtcars]{mtcars}}
"mtcars_df2"
