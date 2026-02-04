# Validate ----------------------------------------------------------------


.stop_bad_input <- function(wb, content, table_name = NULL) {
  if (!inherits(wb, "wbWorkbook")) {
    stop("'wb' must be an openxlsx2 wbWorkbook-class object.")
  }

  if (!is.null(table_name) &&
      !inherits(table_name, "character") &&
      length(table_name != 1)
  ) {
    stop("'table_name' must be a string of length 1")
  }
}


# Detect meta elements ----------------------------------------------------

.has_blanks_message <- function(content, tab_title) {
  blank_cells_message <- content[content$tab_title == tab_title, "blank_cells"][[1]]

  if (!is.na(blank_cells_message)) {
    TRUE
  } else {
    FALSE
  }
}

.has_source <- function(content, tab_title) {
  table_source <- content[content$tab_title == tab_title, "source"][[1]]

  if (!is.na(table_source)) {
    TRUE
  } else {
    FALSE
  }
}

.has_custom_rows <- function(content, tab_title) {
  custom_rows <- content[content$tab_title == tab_title, "custom_rows"][[1]]

  if (any(!is.na(custom_rows))) {
    TRUE
  } else {
    FALSE
  }
}

.has_notes <- function(content, tab_title) {
  table_names <- names(content[content$tab_title == tab_title, "table"][[1]])

  has_header_notes <- any(grepl("(?<=\\[).*(?=\\])", table_names, perl = TRUE))

  has_notes_column <- any(tolower(table_names) %in% "notes")

  any(has_header_notes, has_notes_column)
}

.extract_note_values <- function(content, tab_title) {
  has_notes <- .has_notes(content, tab_title)

  if (has_notes) { # if there are notes in this table

    # Isolate named table dataframe

    table <- content[content$tab_title == tab_title, "table"][[1]]

    # Vector with potential note values, e.g. '[1, 2]' to c(1, 2)

    table_names <- names(table)
    notes_column_index <- which(tolower(table_names) %in% "notes")
    notes_column_content <- table[, notes_column_index]

    possible_note_text <- c(table_names, notes_column_content)

    square_bracket_contents <- unlist(
      regmatches(
        possible_note_text,
        gregexpr("(?<=\\[).*(?=\\])", possible_note_text, perl = TRUE)
      )
    )

    sort(
      as.numeric(
        unique(
          unlist(
            lapply(
              square_bracket_contents,
              function(x) unlist(regmatches(x, gregexpr("\\d", x, perl = TRUE)))
            )
          )
        )
      )
    )
  }
}


# Table placement ---------------------------------------------------------


.get_start_row_blanks_message <- function(has_notes, start_row = 3) {
  if (has_notes) {
    start_row <- start_row + 1
  }

  start_row
}

.get_start_row_custom_rows <- function(has_notes,
                                       has_blanks_message,
                                       start_row = 3) {
  if (has_notes) {
    start_row <- start_row + 1
  }

  if (has_blanks_message) {
    start_row <- start_row + 1
  }

  start_row
}

.get_start_row_source <- function(content,
                                  tab_title,
                                  has_notes,
                                  has_blanks_message,
                                  has_custom_rows,
                                  start_row = 3) {
  if (has_notes) {
    start_row <- start_row + 1
  }

  if (has_blanks_message) {
    start_row <- start_row + 1
  }

  if (has_custom_rows) {
    custom_rows <- content[content$tab_title == tab_title, "custom_rows"][[1]]
    start_row <- start_row + length(custom_rows)
  }

  start_row
}

.get_start_row_table <- function(content,
                                 tab_title,
                                 has_notes,
                                 has_blanks_message,
                                 has_custom_rows,
                                 has_source,
                                 start_row = 3) {
  if (has_notes) {
    start_row <- start_row + 1
  }

  if (has_blanks_message) {
    start_row <- start_row + 1
  }

  if (has_custom_rows) {
    custom_rows <- content[content$tab_title == tab_title, "custom_rows"][[1]]
    start_row <- start_row + length(custom_rows)
  }

  if (has_source) {
    start_row <- start_row + 1
  }

  start_row
}


# Insert sheet elements ---------------------------------------------------


.insert_title <- function(wb, content, tab_title) {
  sheet_type <- content[content$tab_title == tab_title, "sheet_type"][[1]]
  sheet_title <- content[content$tab_title == tab_title, "sheet_title"][[1]]

  if (sheet_type %in% c("cover", "contents", "notes")) {
    wb$add_data(
      sheet = tab_title,
      x = sheet_title,
      start_col = 1,
      start_row = 1,
      na.strings = ""
    )
  }

  if (sheet_type == "tables") {
    wb$add_data(
      sheet = tab_title,
      x = sheet_title,
      start_col = 1,
      start_row = 1,
      na.strings = ""
    )
  }

  wb
}

.insert_table_count <- function(wb, content, tab_title) {
  table_count <- nrow(content[content$tab_title == tab_title, ])

  if (table_count < 10) {
    table_count <- switch(as.character(table_count),
      "1"  = "one",
      "2"  = "two",
      "3"  = "three",
      "4"  = "four",
      "5"  = "five",
      "6"  = "six",
      "7"  = "seven",
      "8"  = "eight",
      "9"  = "nine",
    )
  }

  text <- paste(
    "This worksheet contains", table_count,
    ifelse(table_count == "one", "table.", "tables.")
  )

  wb$add_data(
    sheet = tab_title,
    x = text,
    start_col = 1,
    start_row = 2, # table count will always be the second row,
    na.strings = ""
  )

  wb
}

.insert_notes_statement <- function(wb, content, tab_title) {
  has_notes <- .has_notes(content, tab_title)

  if (has_notes) {
    text <-
      "This table contains notes, which can be found in the Notes worksheet."

    wb$add_data(
      sheet = tab_title,
      x = text,
      start_col = 1,
      start_row = 3, # notes will always go in row 3 if they exist
      na.strings = ""
    )
  }

  wb
}

.insert_blanks_message <- function(wb, content, tab_title) {
  has_blanks_message <- .has_blanks_message(content, tab_title)

  if (has_blanks_message) {
    blanks_text <- content[content$tab_title == tab_title, "blank_cells"][[1]]
    has_notes <- .has_notes(content, tab_title)
    start_row <- .get_start_row_blanks_message(has_notes)

    wb$add_data(
      sheet = tab_title,
      x = blanks_text,
      start_col = 1,
      start_row = start_row,
      na.strings = ""
    )
  }

  wb
}

.insert_custom_rows <- function(wb, content, tab_title) {
  has_custom_rows <- .has_custom_rows(content, tab_title)

  if (has_custom_rows) {
    custom_rows_text <-
      content[content$tab_title == tab_title, "custom_rows"][[1]]

    custom_rows_text <- lapply(custom_rows_text, .make_hyperlink)

    has_notes <- .has_notes(content, tab_title)
    has_blanks <- .has_blanks_message(content, tab_title)
    start_row <- .get_start_row_custom_rows(has_notes, has_blanks)

    for (i in seq_along(custom_rows_text)) {
      has_hyperlink <- class(custom_rows_text[[i]]) == "hyperlink"

      if (has_hyperlink) {
        wb$add_formula(
          sheet = tab_title,
          x = create_hyperlink(
            text = names(custom_rows_text[[i]]),
            file = custom_rows_text[[i]]
          ),
          dims = wb_dims(cols = 1, rows = start_row + (i - 1))
        )
      }

      if (!has_hyperlink) {
        wb$add_data(
          sheet = tab_title,
          x = custom_rows_text[[i]],
          start_row = start_row + (i - 1),
          na.strings = ""
        )
      }
    }
  }

  wb
}

.insert_source <- function(wb, content, tab_title) {
  has_source <- .has_source(content, tab_title)

  if (has_source) {
    source_text <- content[content$tab_title == tab_title, "source"][[1]]
    source_text <- paste("Source:", source_text)
    source_text <- .make_hyperlink(source_text)

    start_row <- .get_start_row_source(
      content,
      tab_title,
      .has_notes(content, tab_title),
      .has_blanks_message(content, tab_title),
      .has_custom_rows(content, tab_title)
    )

    has_hyperlink <- class(source_text) == "hyperlink"
    if (has_hyperlink) {
      wb$add_formula(
        sheet = tab_title,
        x = create_hyperlink(
          text = names(source_text)[[1]],
          file = source_text
        ),
        dims = wb_dims(cols = 1, rows = start_row)
      )
    } else {
      wb$add_data(
        sheet = tab_title,
        x = source_text,
        start_col = 1,
        start_row = start_row,
        na.strings = ""
      )
    }
  }

  wb
}

.insert_table <- function(wb, content, table_name) {
  table <- content[content$table_name == table_name, ][["table"]][[1]]
  sheet_type <- content[content$table_name == table_name, "sheet_type"][[1]]
  tab_title <- content[content$table_name == table_name, "tab_title"][[1]]

  start_row <- .get_start_row_table(
    content,
    tab_title,
    .has_notes(content, tab_title),
    .has_blanks_message(content, tab_title),
    .has_custom_rows(content, tab_title),
    .has_source(content, tab_title)
  )

  if (!any(.determine_mixed_columns(table))) {
    # no mixed columns so insert table as is
    wb$add_data_table(
      sheet = tab_title,
      x = table,
      table_name = table_name,
      start_col = 1,
      start_row = start_row,
      table_style = "none",
      with_filter = FALSE,
      banded_rows = FALSE,
      na.strings = ""
    )

    output <- list(numeric_columns = NULL,
                   numeric_cells = NULL,
                   currency_units = NULL)

  } else { # mixed columns detected - see notes in .determine_mixed_columns
    # columns to be processed as numeric - see notes in .determine_numeric_columns
    # returned as named
    numeric_columns <- .determine_numeric_columns(table)

    #===========================================================================
    # get currency symbols and cell reference to pass to .style_table
    #===========================================================================

    currency_units <- table[numeric_columns] |>
      # find cells with currency symbols outside brackets
      mutate(across(everything(), \(x) {
        ifelse(grepl(x, pattern = "\\[[^\\]]*\\](*SKIP)(*F)|[[:space:]]*[\u00A3|\u0024|\u20AC|\u00A5]", perl = TRUE),
               # extract currency symbols from cells
               regmatches(
                 x,
                 gregexpr("\\[[^\\]]*\\](*SKIP)(*F)|[[:space:]]*[\u00A3|\u0024|\u20AC|\u00A5]",
                          x,
                          perl = TRUE)
               ),
               NA_character_)
      }),
      across(everything(), \(x) trimws(x, which = "both")),
      across(everything(), \(x) replace_na(as.character(x), "")))

    currencies_pos <-
      wb_dims(
        x = table,
        from_row = start_row,
        cols = names(currency_units)
      )

    currencies_pos <- dims_to_rowcol(currencies_pos)

    currencies_pos <- t(outer(currencies_pos$col, currencies_pos$row, paste0))

    currency_units <-
      data.frame(
        cell_text = currency_units |> unlist(use.names = FALSE),
        cell_pos = currencies_pos |> as.vector()
      ) |>
      filter(.data$cell_text != "")

    #===========================================================================
    # clean mixed and currency columns, removing notes and currency symbols
    #===========================================================================

    # remove currency units from cells to allow values to be processed as numbers
    cleaned_table <- table |>
      mutate(across(all_of(numeric_columns), \(x) replace_na(as.character(x), "")),
             across(all_of(numeric_columns), \(x) {
               gsub(x,
                    pattern = "\\[[^\\]]*\\](*SKIP)(*F)|[\u00A3|\u0024|\u20AC|\u00A5]",
                    replacement = "",
                    perl = TRUE)
             }))

    # extract notes from numeric columns
    notes_table <-
      cleaned_table |>
      mutate(across(all_of(numeric_columns),
                    \(x) {
                          ifelse(str_detect(x,
                                            pattern = "^[[:space:]]*(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])[[:space:]]*$"),
                                 x,
                                 "")}),
      across(all_of(numeric_columns), \(x) replace_na(as.character(x), "")))

    # remove notes from numeric columns
    cleaned_table <-
      cleaned_table |>
      mutate(across(all_of(numeric_columns), \(x) replace_na(as.character(x), "")),
             across(all_of(numeric_columns),
                    \(x) {
                      ifelse(str_detect(x,
                                        pattern = "(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])"),
                             "",
                             str_replace_all(x, "[[[:space:]],]", ""))
                    }),
             across(all_of(numeric_columns), \(x) suppressWarnings(as.numeric(x))))

    #===========================================================================
    # insert cleaned data table into workbook
    #===========================================================================

    wb$add_data_table(
      sheet = tab_title,
      x = cleaned_table,
      table_name = table_name,
      start_col = 1,
      start_row = start_row,
      table_style = "none",
      with_filter = FALSE,
      banded_rows = FALSE,
      na.strings = ""
    )

    #===========================================================================
    # insert notes into mixed columns
    #===========================================================================

    notes_pos <-
      wb_dims(
        x = table,
        from_row = start_row,
        cols = numeric_columns
      )

    notes_pos <- dims_to_rowcol(notes_pos)

    notes_pos <- t(outer(notes_pos$col,
                         notes_pos$row,
                         paste0))

    notes_replacement <-
      data.frame(
        cell_text = notes_table[numeric_columns] |>
          unlist(use.names = FALSE),
        cell_pos = notes_pos |> as.vector()
      ) |>
      filter(.data$cell_text != "")

    notes_replacement |>
      pwalk(\(cell_text,
              cell_pos) {
        wb$add_data(
          sheet = tab_title,
          x = cell_text,
          dims = cell_pos,
          col_names = FALSE,
          row_names = FALSE,
          apply_cell_style = FALSE
        )
      })

    #===========================================================================
    # get cell references of cells in numeric columns to pass to .style_table
    #===========================================================================

    numeric_cells <-
      wb_dims(
        x = cleaned_table,
        from_row = start_row,
        cols = numeric_columns
      )

    numeric_cells <- dims_to_rowcol(numeric_cells)

    numeric_cells <- t(outer(numeric_cells$col,
                             numeric_cells$row,
                             paste0)) |>
      as.vector()

    # don't format numeric cells that are going to be formatted as currencies
    numeric_cells <-
      numeric_cells[!numeric_cells %in% currency_units$cell_pos]

    #===========================================================================
    # create output to pass to .style_table
    #===========================================================================

    output <- list(numeric_columns = numeric_columns,
                   numeric_cells = numeric_cells,
                   currency_units = currency_units)
  }

  output

}

# Special case to insert cover-page info, depending on whether it's provided as
# a df or list. All other tables in a workbook are provided as df only.
.insert_cover_table <- function(wb, content, table_name) {
  table <- content[content$table_name == "cover", ][["table"]][[1]]
  tab_title <- content[content$table_name == "cover", "tab_title"][[1]]

  if (inherits(table, "data.frame")) {
    table <- stats::setNames(
      as.list(table[["subsection_content"]]),
      table[["subsection_title"]]
    )
  }

  if (inherits(table, "list")) {
    table <- unlist(c(rbind(names(table), table)))
  }

  table_with_links <- lapply(table, .make_hyperlink)

  for (i in seq_along(table_with_links)) {
    has_hyperlink <- class(table_with_links[[i]]) == "hyperlink"

    if (has_hyperlink) {
      wb$add_formula(
        sheet = tab_title,
        x = create_hyperlink(
          text = names(table_with_links[[i]]),
          file = table_with_links[[i]]
        ),
        dims = wb_dims(cols = 1, rows = i + 1)
      )
    }

    if (!has_hyperlink) {
      wb$add_data(
        sheet = tab_title,
        x = table_with_links[[i]],
        start_row = i + 1,
        na.strings = ""
      )
    }
  }
}


# Handle hyperlinks -------------------------------------------------------


.detect_hyperlink <- function(string) {
  hyper_rx <- "\\[(([[:graph:]]|[[:space:]])+)\\]\\([[:graph:]]+\\)"
  grepl(hyper_rx, string)
}

.detect_multi_hyperlink <- function(string) {
  md_rx <- "\\[(([[:graph:]]|[[:space:]])+?)\\]\\([[:graph:]]+?\\)"
  md_match <- gregexpr(md_rx, string, perl = TRUE)
  md_extract <- regmatches(string, md_match)[[1]]
  has_multi_hyperlink <- length(md_extract) > 1

  if (has_multi_hyperlink) {
    warning(
      "String has more than one hyperlink, only first will be extracted.",
      call. = FALSE
    )
  }

  invisible(has_multi_hyperlink)
}

.check_scheme <- function(string) {
  scheme_rx <- paste("((http(s?)|ftp)://?)", "(mailto:?)", sep = "|")
  grepl(scheme_rx, string)
}

.extract_hyperlink <- function(string, keep_full_string = TRUE) {
  md_rx <- "\\[(([[:graph:]]|[[:space:]])+?)\\]\\([[:graph:]]+?\\)"
  md_match <- regexpr(md_rx, string, perl = TRUE)
  md_extract <- regmatches(string, md_match)[[1]]

  url_rx <- "(?<=\\]\\()([[:graph:]]|[[:space:]])+(?=\\))"
  url_match <- regexpr(url_rx, md_extract, perl = TRUE)
  url_extract <- regmatches(md_extract, url_match)[[1]]

  string_rx <- "(?<=\\[)([[:graph:]]|[[:space:]])+(?=\\])"
  string_match <- regexpr(string_rx, md_extract, perl = TRUE)
  string_extract <- regmatches(md_extract, string_match)[[1]]

  if (keep_full_string) {
    string_extract <- gsub(md_rx, string_extract, string)
  }

  named_hyperlink <- stats::setNames(url_extract, string_extract)
  class(named_hyperlink) <- "hyperlink"
  named_hyperlink
}

.make_hyperlink <- function(string) {
  has_hyperlink <- .detect_hyperlink(string)

  if (has_hyperlink) {
    .detect_multi_hyperlink(string)
    scheme_is_ok <- .check_scheme(string)

    if (scheme_is_ok) {
      string <- .extract_hyperlink(string)
    }
  }

  string
}


# Add sheets to workbook --------------------------------------------------


.add_tabs <- function(wb, content) {
  .stop_bad_input(wb, content)

  for (i in unique(content$tab_title)) {
    wb$add_worksheet(i)
  }

  wb
}

.add_cover <- function(wb, content) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "cover", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "cover", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_cover_table(wb, content, table_name) # rather than .insert_table

  styles <- .style_paragraph()
  fonts <- .style_font()
  .style_sheet_title(wb, tab_title, styles, fonts)
  .style_cover(wb, content, styles, fonts)
  # TODO: needs special handling if list provided
  wb
}


.add_contents <- function(wb, content) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "contents", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "contents", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  table_format <- .insert_table(wb, content, table_name)

  styles <- .style_paragraph()
  fonts <- .style_font()
  .style_sheet_title(wb, tab_title, styles, fonts)
  .style_table(wb, content, table_name, styles, fonts, table_format)
  .style_contents(wb, content, styles)

  wb
}


.add_notes <- function(wb, content) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "notes", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "notes", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  table_format <- .insert_table(wb, content, table_name)

  styles <- .style_paragraph()
  fonts <- .style_font()
  .style_sheet_title(wb, tab_title, styles, fonts)
  .style_table(wb, content, table_name, styles, fonts, table_format)
  .style_notes(wb, content, styles)

  wb
}

.add_tables <- function(wb, content, table_name) {
  .stop_bad_input(wb, content, table_name)

  tab_title <- content[content$table_name == table_name, "tab_title"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_source(wb, content, tab_title)
  .insert_notes_statement(wb, content, tab_title)
  .insert_blanks_message(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  table_format <- .insert_table(wb, content, table_name)

  styles <- .style_paragraph()
  fonts <- .style_font()
  .style_sheet_title(wb, tab_title, styles, fonts)
  .style_table(wb, content, table_name, styles, fonts, table_format)

  wb
}

.determine_empty_cells <- function(table) {

  empty_cells <- table |>
    mutate(across(everything(), \(x) is.na(x) | x == ""))

  empty_cells
}

.determine_currency_cells <- function(table) {
  currency_cells <-
    table |>
    mutate(across(everything(),
                  \(x) {
                    grepl(x,
                          pattern = "^\\s*(?:[-\u2212]?\\s*[\u00A3|\u0024|\u20AC|\u00A5]\\s*[-\u2212]?\\s*(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?|[-\u2212]?\\s*(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?\\s*[\u00A3|\u0024|\u20AC|\u00A5]\\s*[-\u2212]?)\\s*$",
                          perl = TRUE)
                  }))

  currency_cells
}

.determine_numeric_cells <- function(table) {
  numeric_cells <-
    table |>
    mutate(across(everything(),
                  \(x) {
                    grepl(x,
                          pattern = "^[[:space:]]*[-]?[[:space:]]*((?:\\s*)(?:\\d{1,3}(?:,\\d{3})*|\\d+)(?:\\.\\d+)?(?:\\s*))$",
                          perl = TRUE)
                  }))

  numeric_cells
}

.determine_note_cells <- function(table) {
  note_cells <-
    table |>
    mutate(across(everything(),
                  \(x) {
                    grepl(x,
                          pattern = "^[[:space:]]*(\\[[^\\]]*\\].*\\[[^\\]]*\\]|\\[[^\\]]*\\])[[:space:]]*$",
                          perl = TRUE)
                  }))

  note_cells
}

.determine_mixed_columns <- function(table) {

  # number of cells in each column of each type
  numeric_cells <- .determine_numeric_cells(table)

  empty_cells <- .determine_empty_cells(table)

  note_cells <-  .determine_note_cells(table)

  # total number of cells in each column of each type
  note_cells_count <- sapply(note_cells, function(x) sum(x))
  empty_cells_count <- sapply(empty_cells, function(x) sum(x))
  numeric_cells_count <- sapply(numeric_cells, function(x) sum(x))

  # mixed columns are columns which have at least one note and are not entirely
  # notes and empty cells - these are note columns and should be treated as
  # characters
  # currencies are not counted as mixed columns, they are processed
  # as numeric columns
  # a column will only be counted if all the cells can be classified as one
  # of the data types, to avoid cases of unusual data being treated as mixed
  mixed_columns <-
    (
      note_cells_count > 0 &
      (note_cells_count + empty_cells_count) != nrow(table) &
      (numeric_cells_count +
       empty_cells_count +
       note_cells_count) == nrow(table)
    ) |>
    as.vector()

  mixed_columns

}

.determine_numeric_columns <- function(table) {

  # number of cells in each column of each type
  currency_cells <- .determine_currency_cells(table)

  numeric_cells <- .determine_numeric_cells(table)

  empty_cells <- .determine_empty_cells(table)

  note_cells <-  .determine_note_cells(table)

  # total number of cells in each column of each type
  currency_cells_count <- sapply(currency_cells, function(x) sum(x))
  numeric_cells_count <- sapply(numeric_cells, function(x) sum(x))
  note_cells_count <- sapply(note_cells, function(x) sum(x))
  empty_cells_count <- sapply(empty_cells, function(x) sum(x))

  # columns which should be treated as numeric should be currency or numeric
  # they could have any number of note cells but should not be entirely note
  # or empty cells - they would be note columns and should be treated as characters
  # a column will only be counted if all the cells can be classified as one
  # of the data types, to avoid cases of unusual data being treated as numeric
  numeric_columns <-
    (
      (currency_cells_count >= 1 | numeric_cells_count >= 1 | note_cells_count >= 1) &
      (note_cells_count + empty_cells_count) != nrow(table) &
      (currency_cells_count +
       numeric_cells_count +
       note_cells_count +
       empty_cells_count) == nrow(table)
    )

  names(numeric_columns[numeric_columns])
}

.extract_numeric_values <- function(values) {
  numeric_values <-
    values[
      sapply(values,
             grepl,
             pattern = "^[[:space:]]*[-]?[[:space:]]*(?:\\s*)(?:\\d{1,3}(?:,\\d{3})*|\\d+)(?:\\.\\d+)?(?:\\s*)$",
             perl = TRUE)
    ]

  numeric_values <- str_replace_all(numeric_values, "[,[[:space:]]]", "")

  numeric_values <- as.numeric(numeric_values)

  numeric_values
}
