#' A Demo 'data.frame' Object
#'
#' A pre-created data.frame ready to be converted to an aftables-class object
#' with \code{\link{as_aftable}} and then an 'openxlsx' Workbook-class object
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

#' A Demo 'aftables' Object
#'
#' A pre-created 'aftables' object ready to be converted to an 'openxlsx'
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
"demo_aftable"

#' A Demo 'Workbook' Object
#'
#' A pre-created 'openxlsx' Workbook'-class object generated from an
#' aftables-class object with \code{\link{generate_workbook}}.
#'
#' @format An 'openxlsx' Workbook-class object with 5 sheets.
"demo_workbook"
