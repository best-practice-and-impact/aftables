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
.style_table <- function(wb, content, table_name, style_ref, font_ref, wb_config) {
  content_row <- content[content[["table_name"]] == table_name, ]
  table <- content_row[, "table"][[1]]
  tab_title <- content_row[, "tab_title"][[1]]
  sheet_type <- content_row[, "sheet_type"][[1]]

  if (!is.null(wb_config$workbook_format$decimal_places)) {
    decimal_places = wb_config$workbook_format$decimal_places
  } else {
    decimal_places = NULL
  }

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

  # Some columns may contain numbers but have suppression text in them, e.g.
  # '[c]', which makes the column character class. Find the likely numeric cols.
  cols_numeric <- .determine_numeric_columns(table)
  likely_num_cols <- names(Filter(isTRUE, cols_numeric)) # return names of columns that are most likely numeric
  num_cols_index <- which(names(table) %in% likely_num_cols) # get the index of columns that are likely numeric, so styles can be applied

  cols_currency <- .determine_currency_columns(table)
  likely_currency_cols <- names(Filter(isTRUE, cols_currency)) # return names of columns that are most likely numeric currencies
  currency_cols_index <- which(names(table) %in% likely_currency_cols) # get the index of columns that are likely numeric currencies, so styles can be applied

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

  if (length(num_cols_index[!is.na(num_cols_index)])) { # only run if needed
    wb$add_cell_style(
      sheet = tab_title,
      dims = wb_dims(
        rows = seq(start_row, start_row + table_height),
        cols = num_cols_index
      ),
      horizontal = style_ref[["ralign"]]
    )
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

  table_currencies_check <-
    !is.na(table[sort(c(num_cols_index, currency_cols_index))] |>
             mutate(across(everything(), \(x) str_extract(x, "^[\u00A3|$|\u20AC]")))) |>
    as.vector()

  tables_notes_check <-
    !is.na(table[sort(c(num_cols_index, currency_cols_index))] |>
             mutate(across(everything(), \(x) str_extract(x, "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$")))) |>
    as.vector()

  tables_numbers_check <-
    !is.na(
      table[sort(c(num_cols_index, currency_cols_index))] |>
        mutate(
          across(everything(), \(x) str_replace(x, "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$", "")),
          across(everything(), \(x) str_replace(x, "^[\u00A3|$|\u20AC]", "")),
          across(everything(), \(x) as.numeric(x))
        )
    ) |>
    as.vector()

  # custom format if the cell is numeric and has currency/note symbol
  custom_format <- (table_currencies_check | tables_notes_check) & tables_numbers_check

  # standard format if the cell is numeric without currency and notes
  standard_format <- tables_numbers_check & !table_currencies_check & !tables_notes_check

  # get table position on sheet
  table_info <- wb_get_tables(wb, sheet = tab_title)
  table_pos <- table_info$tab_ref[table_info$tab_name == table_name]

  # get anchor position of table
  first_value_cell <- dims_to_dataframe(table_pos, fill = TRUE)[1, 1]
  # get position to update: multiple columns selected
  table_pos <- wb_dims(
    x = table,
    from_dims =
      first_value_cell,
    cols = names(table[sort(c(num_cols_index, currency_cols_index))])
  )

  # get the entire table by cell references
  table_pos <- dims_to_rowcol(table_pos)

  table_pos <- c(t(outer(table_pos$col, table_pos$row, paste0)))

  if (any(custom_format)) {
    custom_format_values <-
      unlist(table[sort(c(num_cols_index, currency_cols_index))],
        use.names = FALSE
      )[custom_format]

    # replacing all custom format values with just their numeric values
    numbers_pos <- table_pos[custom_format]
    table_numbers <- str_replace(custom_format_values, "[\u00A3|$|\u20AC]", "") |>
      str_replace("(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$", "")

    numbers_to_insert <- data.frame(
      cell_text = table_numbers,
      cell_pos = numbers_pos
    )

    numbers_to_insert |>
      pwalk(\(cell_text, cell_pos) {
        wb$add_data(
          sheet = tab_title,
          x = as.numeric(cell_text),
          dims = cell_pos,
          col_names = FALSE,
          row_names = FALSE,
          apply_cell_style = FALSE
        )
      })

    # custom number formats to apply
    new_formats <- data.frame(
      dims = table_pos[custom_format],
      numfmt = paste0(
        replace_na(str_extract(custom_format_values, "[\u00A3|$|\u20AC]"), ""),
        "#,##0.00", # TODO replace with decimal places calculation
        ifelse(str_detect(custom_format_values, "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$"), " &quot;", ""),
        replace_na(str_extract(custom_format_values, "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$"), ""),
        ifelse(str_detect(custom_format_values, "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])$"), "&quot;", "")
      )
    )

    new_formats |>
      pwalk(\(dims, numfmt) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = dims,
          numfmt = numfmt
        )
      })
  }

  if (any(standard_format)) {

    standard_format_values <-
      unlist(table[sort(c(num_cols_index, currency_cols_index))],
        use.names = FALSE
      )[standard_format]

    numbers_pos <- table_pos[standard_format]
    table_numbers <- as.numeric(standard_format_values)

    numbers_to_insert <- data.frame(
      cell_text = table_numbers,
      cell_pos = numbers_pos
    )

    numbers_to_insert |>
      pwalk(\(cell_text, cell_pos) {
        wb$add_data(
          sheet = tab_title,
          x = as.numeric(cell_text),
          dims = cell_pos,
          col_names = FALSE,
          row_names = FALSE,
          apply_cell_style = FALSE
        )
      })

    # standard number formats to apply
    new_formats <- data.frame(
      dims = table_pos[standard_format],
      numfmt = "#,##0.00" # TODO replace with decimal places calculation
    )

    new_formats |>
      pwalk(\(dims, numfmt) {
        wb$add_numfmt(
          sheet = tab_title,
          dims = dims,
          numfmt = numfmt
        )
      })
  }

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

.determine_decimal_places <- function(x, type) {
  # length zero input
  if (length(x) == 0) {
    return(numeric())
  }

  if (type == "numeric") {
    x <- x |>
      .extract_numeric_values()
  }

  if (type == "currency") {
    x <- x |>
      .extract_currency_values()
  }

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
