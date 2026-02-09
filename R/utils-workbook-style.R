#' Set up a list of common paragraph styles
#' @noRd
.style_paragraph <- function() {
  list(
    lalign = "left",
    ralign = "right",
    wrap_text = TRUE
  )
}

#' Set up a list of common font styles
#' @noRd
.style_font <- function(wb_config) {

  base_font_size <-
    ifelse(!is.null(wb_config$workbook_format$base_font_size),
           wb_config$workbook_format$base_font_size,
           12)

  table_header_size <-
    ifelse(!is.null(wb_config$workbook_format$table_header_size),
           wb_config$workbook_format$table_header_size,
           14)

  sheet_header_size <-
    ifelse(!is.null(wb_config$workbook_format$sheet_header_size),
           wb_config$workbook_format$sheet_header_size,
           16)

  base_font_name <- ifelse(!is.null(wb_config$workbook_format$base_font_name),
                           wb_config$workbook_format$base_font_name,
                           "Arial")

  list(
    bold =  1,
    base_font_size = base_font_size,
    table_header_size = table_header_size,
    sheet_header_size = sheet_header_size,
    name = base_font_name
  )
}

#' Apply Styles to the Whole Workbook
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @noRd

.style_workbook <- function(wb, wb_config) {

  base_font_size <-
    ifelse(!is.null(wb_config$workbook_format$base_font_size),
           wb_config$workbook_format$base_font_size,
           12)

  base_font_name <- ifelse(!is.null(wb_config$workbook_format$base_font_name),
                           wb_config$workbook_format$base_font_name,
                           "Arial")

  wb$set_base_font(
    font_size = base_font_size,
    font_name = base_font_name
  )

  wb
}

#' Apply Styles to a Sheet Title
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with .style_paragraph().
#' @param font_ref List. The font-reference object made with .style_font().
#' @noRd
.style_sheet_title <- function(wb, tab_title, style_ref, font_ref) {
  # Sheet titles are BOLD and 16PT by default
  # .style_font() checks the config.yaml file for user preferences
  # which are then included here in font_ref
  wb$add_font(
    sheet = tab_title,
    dims = "A1",
    size = font_ref[["sheet_header_size"]],
    bold = font_ref[["bold"]],
    name = font_ref[["name"]]
  )

  wb$add_cell_style(
    sheet = tab_title,
    dims = "A1",
    horizontal = style_ref[["lalign"]]
  )

  wb
}

#' Apply Styles to a Table
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param table_name Character. The table to which styles should be applied.
#' @param style_ref List. The style-reference object made with .style_paragraph().
#' @param font_ref List. The font-reference object made with .style_font().
#' @noRd
.style_table <- function(wb, content, table_name, style_ref, font_ref, table_formats, wb_config) {
  content_row <- content[content[["table_name"]] == table_name, ]
  table <- content_row[, "table"][[1]]
  tab_title <- content_row[, "tab_title"][[1]]
  sheet_type <- content_row[, "sheet_type"][[1]]

  if (!is.null(wb_config$workbook_format$cellwidth_default)) {
    cellwidth_default <- wb_config$workbook_format$cellwidth_default
  } else {
    cellwidth_default <- 16
  }

  if (!is.null(wb_config$workbook_format$cellwidth_wider)) {
    cellwidth_wider <- wb_config$workbook_format$cellwidth_wider
  } else {
    cellwidth_wider <- 32
  }

  if (!is.null(wb_config$workbook_format$nchar_break)) {
    nchar_break <- wb_config$workbook_format$nchar_break
  } else {
    nchar_break <- 50
  }

  start_row <- .get_start_row_table(
    content,
    tab_title,
    .has_notes(content, tab_title),
    .has_blanks_message(content, tab_title),
    .has_custom_rows(content, tab_title),
    .has_source(content, tab_title)
  )

  table_height <- nrow(table)
  table_width <- ncol(table)

  cellwidth_default <- 16
  cellwidth_wider <- 32
  nchar_break <- 50

  numeric_cols_names <- table_formats$numeric_columns
  # get the index of columns that are likely numeric, so styles can be applied
  numeric_cols_index <- which(names(table) %in% numeric_cols_names)

  numeric_cells <- table_formats$numeric_cells

  # Find indices of columns that should be wider than default
  is_factor_column <- sapply(table, is.factor) # nchar (below) fails on factors
  table[is_factor_column] <- lapply(table[is_factor_column], as.character)
  wide_cells <- names(Filter(function(x) max(nchar(x)) > nchar_break, table))
  wide_cells_index <- which(names(table) %in% wide_cells)
  wide_headers_index <- which(nchar(names(table)) > nchar_break)
  wide_cols_index <- c(wide_cells_index, wide_headers_index)

  # Table data columns are SET-WIDTH (depending on character length),
  # RIGHT-ALIGNED (if numeric) and WRAPPED

  wb$set_col_widths(
    sheet = tab_title,
    cols = seq(table_width),
    widths = cellwidth_default
  )

  if (length(wide_cols_index[!is.na(wide_cols_index)])) { # only run if needed
    wb$set_col_widths(
      sheet = tab_title,
      cols = wide_cols_index,
      widths = cellwidth_wider
    )
  }

  #=============================================================================
  # format numeric columns and apply numeric formatting cells in mixed columns
  #=============================================================================

  if (length(numeric_cols_index > 0)) { # only run if needed
    wb$add_cell_style(
      sheet = tab_title,
      dims = wb_dims(
        rows = seq(start_row, start_row + table_height),
        cols = numeric_cols_index
      ),
      horizontal = style_ref[["ralign"]]
    )
  }

  if (!is.null(table_formats$numeric_formats)) {
    # apply numeric formatting to numeric cells
    table_formats$numeric_formats |>
      pwalk(\(cell_reference, cell_format) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = cell_reference,
          numfmt = cell_format
        )
      })
  }

  #=============================================================================
  # insert currency symbols as number format
  #=============================================================================

  if (!is.null(table_formats$currency_formats)) {
    # apply numeric formatting to numeric cells
    table_formats$currency_formats |>
      pwalk(\(cell_reference, cell_format) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = cell_reference,
          numfmt = cell_format
        )
      })
  }

  wb$add_cell_style(
    sheet = tab_title,
    dims = wb_dims(
      rows = seq(start_row, start_row + table_height),
      cols = seq(table_width)
    ),
    wrap_text = style_ref[["wrap_text"]]
  )

  # .style_font() checks the config.yaml file for user preferences
  # which are then included here in font_ref

  # Table headers are also BOLD
  wb$add_font(
    sheet = tab_title,
    dims = wb_dims(rows = start_row, cols = seq(table_width)),
    bold = font_ref[["bold"]],
    size = font_ref[["base_font_size"]],
    name = font_ref[["name"]]
  )

  wb
}

#' Apply Styles to the Cover Sheet
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with .style_paragraph().
#' @param font_ref List. The font-reference object made with .style_font().
#' @noRd
.style_cover <- function(wb, content, style_ref, font_ref) {
  content_row <- content[content[["sheet_type"]] == "cover", ]
  tab_name <- content_row[, "tab_title"][[1]]
  table <- content_row[, "table"][[1]]

  # The cover column is SET-WIDTH

  wb$set_col_widths(
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

    wb$add_cell_style(
      sheet = tab_name,
      dims = wb_dims(rows = seq(table_height + 1), cols = 1),
      wrap_text = style_ref[["wrap_text"]]
    )

    # Also identify rows containing section headers
    subheader_rows <- which(table_vec %in% names(table)) + 1
  }

  if (cover_is_df) {
    # The cover column is WRAPPED

    table_height <- nrow(table)

    wb$add_cell_style(
      sheet = tab_name,
      dims = wb_dims(rows = seq(table_height * 2 + 1), cols = 1),
      wrap_text = style_ref[["wrap_text"]]
    )

    # Also identify rows containing section headers
    subheader_rows <- seq(2, table_height * 2, 2)
  }

  # Section header rows also have LARGER ROW HEIGHT, are BOLD and 14PT by default

  wb$set_row_heights(
    sheet = tab_name,
    rows = subheader_rows,
    heights = 34
  )

  wb$add_font(
    sheet = tab_name,
    dims = wb_dims(rows = subheader_rows, cols = 1),
    bold = font_ref[["bold"]],
    size = font_ref[["table_header_size"]],
    name = font_ref[["name"]]
  )

  wb
}

#' Apply Styles to the Contents Sheet
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with .style_paragraph().
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

  wb$set_col_widths(
    sheet = tab_title,
    cols = 1,
    widths = 16
  )

  wb$set_col_widths(
    sheet = tab_title,
    cols = 2,
    widths = 56
  )

  wb$add_cell_style(
    sheet = tab_title,
    dims = wb_dims(rows = seq(start_row, table_height + start_row), cols = seq(table_width)),
    wrap_text = style_ref[["wrap_text"]],
    horizontal = style_ref[["lalign"]]
  )
}

#' Apply Styles to the Notes Sheet
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with .style_paragraph().
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

  wb$set_col_widths(
    sheet = tab_title,
    cols = 1,
    widths = 16
  )

  wb$set_col_widths(
    sheet = tab_title,
    cols = 2,
    widths = 56
  )

  wb$add_cell_style(
    sheet = tab_title,
    dims = wb_dims(rows = seq(start_row, table_height + start_row), cols = seq(table_width)),
    wrap_text = style_ref[["wrap_text"]],
    horizontal = style_ref[["lalign"]]
  )
}
