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

  # Determine cell / column types

  table_datatypes <- .determine_table_datatypes(table)

  numeric_columns <- table_datatypes$numeric_columns

  currency_cells <- table_datatypes$currency_cells

  numeric_cells <- table_datatypes$numeric_cells

  note_cells <- table_datatypes$note_cells


  # Get table cell reference positions

  table_cell_references <-
    wb_dims(
      x = table,
      from_row = start_row,
      cols = names(table)
    )

  table_cell_references <- dims_to_rowcol(table_cell_references)

  table_cell_references <- t(outer(table_cell_references$col, table_cell_references$row, paste0))

  colnames(table_cell_references) <- names(table)


  if (length(numeric_columns) > 0) {

    #===========================================================================
    # Clean mixed columns by removing notes
    #===========================================================================

    note_values <- table[note_cells]
    note_cell_references <- table_cell_references[note_cells]

    notes_replacement <- data.frame(
      cell_reference = note_cell_references,
      cell_text = note_values
    )

    table[note_cells] <- ""


    #===========================================================================
    # Extract currency symbols from numeric columns for number formatting
    #===========================================================================
    currency_units <- .extract_currency_units(table, numeric_columns)

    #===========================================================================
    # Clean table removing currency symbols
    #===========================================================================
    table <- .replace_currency_units(table, numeric_columns)

    #===========================================================================
    # Convert numeric and currency columns to numeric
    #===========================================================================
    table <- .clean_numeric_data(table, numeric_columns)

    #===========================================================================
    # Determine decimal places from data for number formatting
    #===========================================================================
    decimal_places <- .determine_decimal_places(table, numeric_columns)

    #===========================================================================
    # Create number formats from currency units
    # and decimal places to pass to .style_table
    #===========================================================================

    number_cell_references <-
      table_cell_references[, numeric_columns, drop = FALSE]

    number_formats <- .determine_number_formats(
      table,
      currency_units,
      decimal_places,
      number_cell_references
    )

  } else {
    notes_replacement <-
      data.frame(
        cell_reference = character(0),
        cell_text = character(0)
      )

    number_formats <- NULL
  }

  #=============================================================================
  # insert data table into workbook
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
    number_formats = number_formats
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

.add_cover <- function(wb, content, font_ref) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "cover", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "cover", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_cover_table(wb, content, table_name) # rather than .insert_table

  styles <- .style_paragraph()
  .style_sheet_title(wb, tab_title, styles, font_ref)
  .style_cover(wb, content, styles, font_ref)
  # TODO: needs special handling if list provided
  wb
}


.add_contents <- function(wb, content, font_ref, workbook_format) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "contents", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "contents", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  table_format <- .insert_table(wb, content, table_name)

  styles <- .style_paragraph()
  .style_sheet_title(wb, tab_title, styles, font_ref)
  .style_table(wb, content, table_name, styles, font_ref, table_format, workbook_format)
  .style_contents(wb, content, styles)

  wb
}


.add_notes <- function(wb, content, font_ref, workbook_format) {
  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "notes", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "notes", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  table_format <- .insert_table(wb, content, table_name)

  styles <- .style_paragraph()
  .style_sheet_title(wb, tab_title, styles, font_ref)
  .style_table(wb, content, table_name, styles, font_ref, table_format, workbook_format)
  .style_notes(wb, content, styles)

  wb
}

.add_tables <- function(wb, content, table_name, font_ref, workbook_format) {
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
  .style_sheet_title(wb, tab_title, styles, font_ref)
  .style_table(wb, content, table_name, styles, font_ref, table_format, workbook_format)

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
  old <- options(scipen = 999)
  on.exit(options(old), add = TRUE)

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
  # if all currency symbols and notes were removed
  currency_cells[!numeric_columns] <- FALSE

  # valid numeric cells are only those in columns which could be numeric
  # if all notes were removed
  numeric_cells[!numeric_columns] <- FALSE

  # valid note cells are only those in mixed columns which could be numeric
  # if all notes were removed
  note_cells[!numeric_columns] <- FALSE


  output <- list(
    numeric_columns = names(numeric_columns[numeric_columns]),
    currency_cells = as.matrix(currency_cells),
    numeric_cells = as.matrix(numeric_cells),
    note_cells = as.matrix(note_cells)
  )

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

.extract_currency_units <- function(table,
                                    numeric_columns) {

  output <- table |>
    dplyr::select(all_of(numeric_columns)) |>
    mutate(
      across(
        everything(),
        \(x) {
          tidyr::replace_na(str_extract(x, extract_currency_symbol_regex), "")
        }
      )
    )

  output

}

.replace_currency_units <- function(table,
                                    numeric_columns) {

  output <- table |>
    mutate(
      across(
        where(\(x) !is.numeric(x)) & all_of(numeric_columns),
        \(x) {
          str_replace(x, extract_currency_symbol_regex, "")
        }
      )
    )

  output
}

.clean_numeric_data <- function(table,
                                numeric_columns) {

  output <- table |>
    mutate(
      across(
        where(\(x) !is.numeric(x)) & all_of(numeric_columns),
        \(x) as.numeric(str_replace_all(x, "[\\s,]", ""))
      )
    )

  output
}

.set_workbook_properties <- function(wb, content) {

  if (!is.null(content$keywords)) {
    content$keywords <- paste0(content$keywords, collapse = ", ")
  }

  wb$set_properties(
    creator = content$author,
    title = content$title,
    subject = content$subject,
    category = content$category,
    keywords = content$keywords,
    comments = content$comments
  )

  wb
}

.determine_decimal_places <- function(table, numeric_columns) {

  # switch off scientific notation
  old <- options(scipen = 999)
  on.exit(options(old), add = TRUE)

  output <- table |>
    dplyr::select(all_of(numeric_columns)) |>
    mutate(
      across(everything(), \(x) nchar(abs(x)) - 1 - nchar(floor(abs(x)))),
      across(everything(), \(x) ifelse(x < 0, 0, x))
    ) |>
    dplyr::summarise(across(everything(), \(x) max(x, na.rm = TRUE)))

  output

}

.determine_number_formats <- function(df,
                                      currency_units,
                                      decimal_places,
                                      number_cell_references) {


  numeric_columns <- colnames(number_cell_references)

  number_helper_function_check <-
    length(
      sapply(df, attr, which = "aftables_decimal_places") |>
        unlist(use.names = FALSE)
    ) > 0 |
    length(
      sapply(df, attr, which = "aftables_thousand_separators") |>
        unlist(use.names = FALSE) > 0
    )

  # if number_formatter helper function has been used
  if (number_helper_function_check) {
    # extract decimal places set by helper function
    user_decimal_places <- purrr::map(
      df[numeric_columns],
      \(x) attr(x, "aftables_decimal_places", exact = TRUE)
    ) |>
      unlist()

    # replace decimal places determined from data with user decimal places
    decimal_places[names(decimal_places) %in% names(user_decimal_places)] <-
      user_decimal_places

    # extract thousand separators set by helper function
    thousand_separators <- purrr::map(
      df[numeric_columns],
      \(x) attr(x, "aftables_thousand_separators", exact = TRUE)
    ) |>
      purrr::map(\(x) ifelse(is.null(x), TRUE, x)) |>
      tidyr::as_tibble()

    # expand thousand_separators by row to cover entire table
    thousand_separators <-
      tidyr::uncount(thousand_separators, nrow(df)) |>
      unlist(use.names = FALSE)

  } else {
    thousand_separators <- FALSE
  }

  # expand decimal_places by row to cover entire table
  decimal_places <-
    tidyr::uncount(decimal_places, nrow(df)) |>
    unlist(use.names = FALSE)

  cell_format_options <- tibble(
    currency_units = unlist(currency_units, use.names = FALSE),
    decimal_places = decimal_places,
    thousand_separators = thousand_separators
  )

  output <- list(
    cell_reference = as.vector(number_cell_references),
    cell_format = cell_format_options |>
      mutate(
        format = paste0(
          currency_units,
          ifelse(thousand_separators, "#,##0", "###0"),
          ifelse(decimal_places > 0, ".", ""),
          mapply(
            paste0,
            mapply(rep,
                   "0",
                   times = decimal_places),
            collapse = ""
          )
        )
      ) |>
      dplyr::select(format) |>
      unlist(use.names = FALSE)
  )

  output
}
