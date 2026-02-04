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
.style_font <- function() {
  list(
    bold =  1,
    pt12 = 12,
    pt14 = 14,
    pt16 = 16,
    name = "Arial"
  )
}

#' Apply Styles to the Whole Workbook
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @noRd

.style_workbook <- function(wb) {
  wb$set_base_font(
    font_size = 12,
    font_name = "Arial"
  )

  wb
}

#' Apply Styles to a Sheet Title
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_paragraph()].
#' @param font_ref List. The font-reference object made with [.style_font()].
#' @noRd
.style_sheet_title <- function(wb, tab_title, style_ref, font_ref) {
  # Sheet titles are BOLD and 16PT

  wb$add_font(
    sheet = tab_title,
    dims = "A1",
    size = font_ref[["pt16"]],
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
#' @param style_ref List. The style-reference object made with [.style_paragraph()].
#' @param font_ref List. The font-reference object made with [.style_font()].
#' @noRd
.style_table <- function(wb, content, table_name, style_ref, font_ref, table_formats) {
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
  table_width <- ncol(table)

  cellwidth_default <- 16
  cellwidth_wider <- 32
  nchar_break <- 50

  numeric_cols_names <- table_formats$numeric_columns
  numeric_cols_index <- which(names(table) %in% numeric_cols_names) # get the index of columns that are likely numeric, so styles can be applied

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

  if (length(numeric_cols_index[!is.na(numeric_cols_index)])) { # only run if needed
    wb$add_cell_style(
      sheet = tab_title,
      dims = wb_dims(
        rows = seq(start_row, start_row + table_height),
        cols = numeric_cols_index
      ),
      horizontal = style_ref[["ralign"]]
    )

    # apply numeric formatting to numeric cells
    formats_to_apply <-
      data.frame(
        numfmt = "#,##0.00",
        dims = numeric_cells
      )

    formats_to_apply |>
      pwalk(\(dims, numfmt) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = dims,
          numfmt = numfmt
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

  # Table headers are also BOLD
  wb$add_font(
    sheet = tab_title,
    dims = wb_dims(rows = start_row, cols = seq(table_width)),
    bold = font_ref[["bold"]],
    size = font_ref[["pt12"]],
    name = font_ref[["name"]]
  )

  #=============================================================================
  # insert currency symbols as number format
  #=============================================================================

  if (!is.null(table_formats$currency_units)) {

    table_formats$currency_units |>
      mutate(cell_text = paste0(.data$cell_text, "#,##0.00")) |>
      pwalk(\(cell_pos, cell_text) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = cell_pos,
          numfmt = cell_text
        )
      })

  }

  wb
}

#' Apply Styles to the Cover Sheet
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_paragraph()].
#' @param font_ref List. The font-reference object made with [.style_font()].
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

  # Section header rows also have LARGER ROW HEIGHT, are BOLD and 14PT

  wb$set_row_heights(
    sheet = tab_name,
    rows = subheader_rows,
    heights = 34
  )

  wb$add_font(
    sheet = tab_name,
    dims = wb_dims(rows = subheader_rows, cols = 1),
    bold = font_ref[["bold"]],
    size = font_ref[["pt14"]],
    name = font_ref[["name"]]
  )

  wb
}

#' Apply Styles to the Contents Sheet
#' @param wb An 'openxlsx2' wbWorkbook object.
#' @param tab_title Character. The tab in `wb` where the style should be set.
#' @param style_ref List. The style-reference object made with [.style_paragraph()].
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
#' @param style_ref List. The style-reference object made with [.style_paragraph()].
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

.determine_decimal_places <- function(x) {
  # length zero input
  if (length(x) == 0) {
    return(numeric())
  }

  x <- x |>
    .extract_numeric_values()

  # count decimals
  x_nchr <- x |>
    abs() |>
    as.character() |>
    nchar() |>
    as.numeric()
  x_int <- floor(x) |>
    abs() |>
    nchar()
  x_nchr <- x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] <- 0

  max(x_nchr, na.rm = TRUE)
}
