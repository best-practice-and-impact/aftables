#' Set Up a List of Common Styles
#' @noRd
.style_create <- function() {

  list(
    bold   = openxlsx::createStyle(textDecoration = "Bold"),
    pt14   = openxlsx::createStyle(fontSize = 14),
    pt16   = openxlsx::createStyle(fontSize = 16),
    lalign = openxlsx::createStyle(halign = "left"),
    ralign = openxlsx::createStyle(halign = "right"),
    wrap   = openxlsx::createStyle(wrapText = TRUE)
  )

}

#' Apply Styles to the Whole Workbook
#' @param wb An 'openxlsx' Workbook object.
#' @noRd
.style_workbook <- function(wb) {

  openxlsx::modifyBaseFont(
    wb = wb,
    fontSize = 12,
    fontName = "Arial"
  )

  wb

}

#' Apply Styles to a Sheet Title
#' @param wb An 'openxlsx' Workbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_create].
#' @noRd
.style_sheet_title <- function(wb, tab_title, style_ref) {

  # Sheet titles are BOLD and 16PT

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = 1,
    cols = 1,
    style = style_ref[["bold"]],
    stack = TRUE
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = 1,
    cols = 1,
    style = style_ref[["pt16"]],
    stack = TRUE
  )

  wb

}

#' Apply Styles to a Table
#' @param wb An 'openxlsx' Workbook object.
#' @param table_name Character. The table to which styles should be applied.
#' @param style_ref List. The style-reference object made with [.style_create].
#' @noRd
.style_table <- function(wb, content, table_name, style_ref) {

  content_row <- content[content[["table_name"]] == table_name, ]
  table <- content_row[, "table"][[1]]
  tab_title <- content_row[, "tab_title"][[1]]
  sheet_type <- content_row[, "sheet_type"][[1]]

  start_row <- .get_start_row_table(
    content,
    tab_title,
    .has_notes(content, tab_title),
    .has_blanks_message(content, tab_title),
    .has_custom_rows(content, tab_title),
    .has_source(content, tab_title)
  )

  table_height <- nrow(table)
  table_width  <- ncol(table)

  cellwidth_default <- 16
  cellwidth_wider <- 32
  nchar_break <- 50

  # Some columns may contain numbers but have suppression text in them, e.g.
  # '[c]', which makes the column character class. Find the likely numeric cols.
  cols_numeric <- suppressWarnings(lapply(table, as.numeric))  # coerce columns to numeric
  cols_numeric <- lapply(cols_numeric, function(x) any(!is.na(x)))  # at least one number after coercion?
  likely_num_cols <- names(Filter(isTRUE, cols_numeric))  # return names of columns that are most likely numeric
  num_cols_index <- which(names(table) %in% likely_num_cols)  # get the index of columns that are likely numeric, so styles can be applied

  # Find indices of columns that should be wider than default
  is_factor_column <- sapply(table, is.factor)   # nchar (below) fails on factors
  table[is_factor_column] <- lapply(table[is_factor_column], as.character)
  wide_cells <- names(Filter(function(x) max(nchar(x)) > nchar_break, table))
  wide_cells_index <- which(names(table) %in% wide_cells)
  wide_headers_index <- which(nchar(names(table)) > nchar_break)
  wide_cols_index <- c(wide_cells_index, wide_headers_index)

  # Table data columns are SET-WIDTH (depending on character length),
  # RIGHT-ALIGNED (if numeric) and WRAPPED

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_title,
    cols = seq(table_width),
    widths = cellwidth_default  # set all columns to default width first
  )

  if (length(wide_cols_index[!is.na(wide_cols_index)])) {  # only run if neded
    openxlsx::setColWidths(
      wb = wb,
      sheet = tab_title,
      cols = wide_cols_index,
      widths = cellwidth_wider  # apply larger width to certain columns
    )
  }

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, start_row + table_height),
    cols = seq(table_width),
    gridExpand = TRUE,
    style = style_ref[["wrap"]],
    stack = TRUE
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, start_row + table_height),
    cols = num_cols_index,
    gridExpand = TRUE,
    style = style_ref[["ralign"]],
    stack = TRUE
  )

  # Table headers are also BOLD

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = start_row,
    cols = seq(table_width),
    style = style_ref[["bold"]],
    stack = TRUE
  )

  wb

}

#' Apply Styles to the Cover Sheet
#' @param wb An 'openxlsx' Workbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_create].
#' @noRd
.style_cover <- function(wb, content, style_ref) {

  content_row <- content[content[["sheet_type"]] == "cover", ]
  tab_name <- content_row[, "tab_title"][[1]]
  table <- content_row[, "table"][[1]]

  # The cover column is SET-WIDTH

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_name,
    cols = 1,
    widths = 72
  )

  # The cover content can be provided as a list or data.frame
  cover_is_list <- inherits(table, "list")
  cover_is_df <- is.data.frame(table)

  if (cover_is_list) {

    table_vec <- unlist(c(rbind(names(table), table)))

    # The cover column is SET-WIDTH and WRAPPED

    table_height <- length(table_vec)

    openxlsx::addStyle(
      wb = wb,
      sheet = tab_name,
      rows = seq(table_height + 1),
      cols = 1,
      style = style_ref[["wrap"]],
      stack = TRUE
    )

    # Also identify rows containing section headers
    subheader_rows <- which(table_vec %in% names(table)) + 1

  }

  if (cover_is_df) {

    # The cover column is WRAPPED

    table_height <- nrow(table)

    openxlsx::addStyle(
      wb = wb,
      sheet = tab_name,
      rows = seq(table_height * 2 + 1),
      cols = 1,
      style = style_ref[["wrap"]],
      stack = TRUE
    )

    # Also identify rows containing section headers
    subheader_rows <- seq(2, table_height * 2, 2)

  }

  # Section header rows also have LARGER ROW HEIGHT, are BOLD and 14PT

  openxlsx::setRowHeights(
    wb = wb,
    sheet = tab_name,
    rows = subheader_rows,
    heights = 34
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_name,
    rows = subheader_rows,
    cols = 1,
    style = style_ref[["bold"]],
    stack = TRUE
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_name,
    rows = subheader_rows,
    cols = 1,
    style = style_ref[["pt14"]],
    stack = TRUE
  )

  wb

}

#' Apply Styles to the Contents Sheet
#' @param wb An 'openxlsx' Workbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_create].
#' @noRd
.style_contents <- function(wb, content, style_ref) {

  tab_title <- content[content[["sheet_type"]] == "contents", "tab_title"][[1]]
  table <- content[content[["sheet_type"]] == "contents", "table"][[1]]

  table_height <- nrow(table)
  table_width <- ncol(table)

  start_row <- .get_start_row_table(
    content,
    tab_title,
    .has_notes(content, tab_title),
    .has_blanks_message(content, tab_title),
    .has_custom_rows(content, tab_title),
    .has_source(content, tab_title)
  )

  # Contents columns are SET-WIDTH, WRAPPED and LEFT ALIGNED

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_title,
    cols = 1,
    widths = 16
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_title,
    cols = 2,
    widths = 56
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, table_height + start_row),
    cols = seq(table_width),
    gridExpand = TRUE,
    style = style_ref[["wrap"]],
    stack = TRUE
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, table_height + start_row),
    cols = seq(table_width),
    gridExpand = TRUE,
    style = style_ref[["lalign"]],
    stack = TRUE
  )

}

#' Apply Styles to the Notes Sheet
#' @param wb An 'openxlsx' Workbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_create].
#' @noRd
.style_notes <- function(wb, content, style_ref) {

  tab_title <- content[content[["sheet_type"]] == "notes", "tab_title"][[1]]
  table <- content[content[["sheet_type"]] == "notes", "table"][[1]]

  table_height <- nrow(table)
  table_width <- ncol(table)

  start_row <- .get_start_row_table(
    content,
    tab_title,
    .has_notes(content, tab_title),
    .has_blanks_message(content, tab_title),
    .has_custom_rows(content, tab_title),
    .has_source(content, tab_title)
  )

  # Notes columns are SET-WIDTH, WRAPPED and LEFT ALIGNED

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_title,
    cols = 1,
    widths = 16
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = tab_title,
    cols = 2,
    widths = 56
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, table_height + start_row),
    cols = seq(table_width),
    gridExpand = TRUE,
    style = style_ref[["wrap"]],
    stack = TRUE
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = tab_title,
    rows = seq(start_row, table_height + start_row),
    cols = seq(table_width),
    gridExpand = TRUE,
    style = style_ref[["lalign"]],
    stack = TRUE
  )

}
