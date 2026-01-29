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

  # get user set decimal places
  if (!is.null(wb_config$workbook_format$decimal_places[[tab_title]])) {
    decimal_places = wb_config$workbook_format$decimal_places[[tab_title]]

    # apply default to all columns
    if (!is.null(decimal_places$default)) {

      default_dp <- decimal_places$default

      decimal_places <- decimal_places[names(decimal_places) != "default"]

      names(decimal_places) <- as.numeric(str_replace_all(names(decimal_places),
                                                          "column", ""))

      names(decimal_places) <- names(table[as.numeric(names(decimal_places))])

      custom_dp <- as.list(rep(default_dp,
                               ncol(table)))

      names(custom_dp) <- names(table)

      # if individual columns set, override defaults
      if (length(decimal_places) > 1) {
        custom_dp[names(decimal_places)] <- decimal_places
      }

      columns_custom_dp <- names(custom_dp)
    }
  } else {
    decimal_places = NULL

    custom_dp <- .determine_decimal_places(table)

    columns_custom_dp <- names(custom_dp)
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

  mixed_cols <- .determine_mixed_columns(table)
  mixed_cols_names <- names(Filter(isTRUE, mixed_cols)) # return names of columns that are most likely numeric
  mixed_cols_index <- which(names(table) %in% mixed_cols_names) # get the index of columns that are likely numeric, so styles can be applied

  # mixed columns and numeric columns need to have formatting applied
  format_cols <- (mixed_cols | .determine_numeric_columns(table))
  format_cols_names <- names(Filter(isTRUE, format_cols)) # return names of columns that are most likely numeric currencies
  format_cols_index <- which(names(table) %in% format_cols_names) # get the index of columns that are likely numeric currencies, so styles can be applied

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

  if (length(mixed_cols_index[!is.na(mixed_cols_index)])) { # only run if needed
    wb$add_cell_style(
      sheet = tab_title,
      dims = wb_dims(
        rows = seq(start_row, start_row + table_height),
        cols = mixed_cols_index
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

  if (length(format_cols_index) > 0) {
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
      cols = names(table[format_cols_index])
    )

    # get the entire table by cell references
    table_pos <- dims_to_rowcol(table_pos)

    table_pos <- c(t(outer(table_pos$col, table_pos$row, paste0)))

    # create the columns to be inserted
    cell_text <- table[format_cols_index] |> unlist(use.names = FALSE)
    cell_pos <- table_pos

    # filter the cell_text to avoid valid note cells and NAs
    cell_pos <- cell_pos[!str_detect(cell_text, pattern = notes_regex)]
    cell_text <- cell_text[!str_detect(cell_text, pattern = notes_regex)]
    cell_pos <- cell_pos[!is.na(cell_text)]
    cell_text <- cell_text[!is.na(cell_text)]

    formats_to_apply <- bind_cols(
      table[format_cols_index] |>
        mutate(across(everything(),
                      \(x) {
                            paste0(replace_na(str_extract(x,
                                                          currency_regex), ""),
                                   ifelse(cur_column() %in% columns_custom_dp,
                                          ifelse(custom_dp[cur_column()] == 0,
                                                 "#,##0",
                                                 paste0("#,##0.", paste0(
                                                   rep("0",
                                                       times = as.numeric(custom_dp[cur_column()])),
                                                   collapse = ""
                                                 )
                                                 )),
                                          "#,##0.00")) })) |>
        pivot_longer(
          cols = everything(),
          cols_vary = "slowest",
          names_to = NULL
        ),
      table_pos,
      .name_repair = "unique_quiet"
    )

    names(formats_to_apply) <- c("numfmt", "dims")

    formats_to_apply |>
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

.determine_decimal_places <- function(x) {

  dp_cols <- (.determine_numeric_columns(x) | .determine_currency_columns(x))

  if (any(dp_cols)) {

    if (is(x, "character")) {
      x_nchr <-
        str_replace(x, notes_regex, "") |>
        str_replace(currency_regex, "") |>
        as.numeric() |>
        abs() |>
        as.character() |>
        nchar() |>
        as.numeric()

      x_int <-
        str_replace(x, notes_regex, "") |>
        str_replace(currency_regex, "") |>
        as.numeric() |>
        floor() |>
        abs() |>
        nchar()

      x_nchr <- x_nchr - 1 - x_int
      x_nchr[x_nchr < 0] <- 0

      output <-
        x_nchr |>
        max(x, na.rm = TRUE) |>
        unique()

    } else if (is(x, "data.frame")) {
      x_nchr <-
        x[dp_cols] |>
        mutate(across(everything(), \(x) str_replace(x, notes_regex, "")),
               across(everything(), \(x) str_replace(x, currency_regex, "")),
               across(everything(), \(x) str_replace(x, ",", "")),
               across(everything(), \(x) trimws(x)),
               across(everything(), \(x) as.numeric(x)),
               across(everything(), \(x) abs(x)),
               across(everything(), \(x) as.character(x)),
               across(everything(), \(x) nchar(x)),
               across(everything(), \(x) as.numeric(x)))

      x_int <-
        x[dp_cols] |>
        mutate(across(everything(), \(x) str_replace(x, notes_regex, "")),
               across(everything(), \(x) str_replace(x, currency_regex, "")),
               across(everything(), \(x) str_replace(x, ",", "")),
               across(everything(), \(x) trimws(x)),
               across(everything(), \(x) as.numeric(x)),
               across(everything(), \(x) floor(x)),
               across(everything(), \(x) abs(x)),
               across(everything(), \(x) nchar(x)))

      x_nchr <- x_nchr - 1 - x_int
      x_nchr[x_nchr < 0] <- 0

      output <- x_nchr |>
        mutate(across(everything(), \(x) max(x, na.rm = TRUE))) |>
        unique()
    }

  } else {
    output <- list()
  }

  output

}
