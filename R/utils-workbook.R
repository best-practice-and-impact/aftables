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

  wb$add_data(
    sheet = tab_title,
    x = sheet_title,
    start_col = 1,
    start_row = 1,
    na.strings = ""
  )

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
  # convert tibbles to data frames before processing
  table <- as.data.frame(content[content$table_name == table_name, ][["table"]][[1]])
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

  # initial cleaning of table regardless of mixed columns
  # currency cells, numeric cells, numeric columns

  table_datatypes <- .determine_table_datatypes(table)

  numeric_columns <- table_datatypes$numeric_columns

  currency_cells <- table_datatypes$currency_cells

  numeric_cells <- table_datatypes$numeric_cells

  note_cells <- table_datatypes$note_cells

  table_cell_references <-
    wb_dims(
      x = table,
      from_row = start_row,
      cols = names(table)
    )

  table_cell_references <- dims_to_rowcol(table_cell_references)

  table_cell_references <- t(outer(table_cell_references$col, table_cell_references$row, paste0))

  #===========================================================================
  # clean mixed columns by removing notes
  #===========================================================================

  # extract notes from mixed columns
  note_values <- table[note_cells]
  note_cell_references <- table_cell_references[note_cells]

  notes_replacement <-
    data.frame(
      cell_reference = note_cell_references,
      cell_text = note_values
    )

  table[note_cells] <- ""

  if (any(currency_cells)) {
    #===========================================================================
    # set currency formats to pass to .style_table
    #===========================================================================

    currency_units <- .extract_currency_units(
      table = table,
      currency_cells = currency_cells
    )

    currencies_cell_references <- table_cell_references[currency_cells]

    currency_formats <-
      data.frame(
        cell_reference = currencies_cell_references,
        cell_format = paste0(currency_units, "#,##0.00")
      )

    #===========================================================================
    # clean table removing currency symbols
    #===========================================================================

    table[currency_cells] <- .replace_currency_units(table, currency_cells)

  } else {
    currency_formats <- NULL
  }

  if (any(numeric_cells)) {
    #===========================================================================
    # set numeric formats to pass to .style_table
    #===========================================================================
    numeric_cell_references <- table_cell_references[numeric_cells]

    numeric_formats <-
      data.frame(
        cell_reference = numeric_cell_references,
        cell_format = "#,##0.00"
      )

  } else {
    numeric_formats <- NULL
  }

  #=============================================================================
  # convert cells and columns to numeric once currency symbols have been removed
  #=============================================================================
  table <- .clean_numeric_data(table, (numeric_cells | currency_cells))

  #=============================================================================
  # insert cleaned data table into workbook
  #=============================================================================

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

  #=============================================================================
  # insert notes into mixed columns
  #=============================================================================

  notes_replacement |>
    pwalk(\(cell_reference,
            cell_text) {
      wb$add_data(
        sheet = tab_title,
        x = cell_text,
        dims = cell_reference,
        col_names = FALSE,
        row_names = FALSE,
        apply_cell_style = FALSE
      )
    })
  #=============================================================================
  # create output to pass to .style_table
  #=============================================================================

  output <- list(
    numeric_columns = numeric_columns,
    numeric_formats = numeric_formats,
    currency_formats = currency_formats
  )

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
    mutate(
      across(
        everything(),
        \(x) {
          grepl(x,
                pattern = detect_currency_regex,
                perl = TRUE)
        }
      )
    )

  currency_cells
}

.determine_numeric_cells <- function(table) {
  numeric_cells <-
    table |>
    mutate(
      across(
        everything(),
        \(x) {
          grepl(x, pattern = numeric_regex, perl = TRUE)
        }
      )
    )

  numeric_cells
}

.determine_note_cells <- function(table) {
  note_cells <-
    table |>
    mutate(
      across(
        everything(),
        \(x) {
          grepl(x, pattern = notes_regex, perl = TRUE)
        }
      )
    )

  note_cells
}

.determine_table_datatypes <- function(table) {

  # switch off scientific notation
  scipen_orig <- getOption("scipen")
  options(scipen = 999)

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

  # columns which should be treated as numeric can contain either currency or
  # numeric data and character data in note format between square brackets []
  # numeric columns can have any number of note cells but should not be
  # entirely note or empty cells
  # columns which are entirely note or empty cells are note columns and should
  # be treated as characters
  # a column will only be counted if all the cells can be classified as one
  # of the data types, to avoid cases of unusual data being treated as numeric
  numeric_columns <-
    (
      (currency_cells_count >= 1 | numeric_cells_count >= 1) &
      (currency_cells_count +
       numeric_cells_count +
       note_cells_count +
       empty_cells_count) == nrow(table)
    )

  # valid currency cells are only those in columns which could be numeric
  # if all currency symbols were removed
  currency_cells[!numeric_columns] <- FALSE
  # valid note cells are only those in mixed columns which could be numeric
  # if all notes were removed
  note_cells[!numeric_columns] <- FALSE

  output <- list(
    numeric_columns = names(numeric_columns[numeric_columns]),
    currency_cells = as.matrix(currency_cells),
    numeric_cells = as.matrix(numeric_cells),
    note_cells = as.matrix(note_cells)
  )

  # restore scientific notation
  options(scipen = scipen_orig)

  output
}

.extract_numeric_values <- function(values) {
  numeric_values <-
    values[
      sapply(
        values,
        grepl,
        pattern = numeric_regex,
        perl = TRUE
      )
    ]

  numeric_values <- str_replace_all(numeric_values, "[\\s,]", "")

  numeric_values <- as.numeric(numeric_values)

  numeric_values
}

.extract_currency_units <- function(table, currency_cells) {
  currency_units <-
    trimws(
      regmatches(
        table[currency_cells],
        gregexpr(
          extract_currency_symbol_regex,
          table[currency_cells],
          perl = TRUE
        )
      ),
      which = "both"
    )

  currency_units
}

.replace_currency_units <- function(table, currency_cells) {

  output <-
    sapply(
      regmatches(
        table[currency_cells],
        gregexpr(
          extract_currency_symbol_regex,
          table[currency_cells],
          perl = TRUE
        ),
        invert = TRUE
      ),
      paste,
      collapse = ""
    )

  output <- str_replace_all(output, "[\\s,]", "")

  output

}

.clean_numeric_data <- function(table, numeric_cells) {
  table[numeric_cells] <- trimws(table[numeric_cells], which = "both")

  table[numeric_cells] <-
    str_replace_all(table[numeric_cells], "[\\s,]", "")

  table <- type.convert(table, as.is = TRUE)

  table
}
