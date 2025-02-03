
# Validate ----------------------------------------------------------------


.stop_bad_input <- function(wb, content, table_name = NULL) {

  if (!inherits(wb, "Workbook")) {
    stop("'wb' must be a Workbook-class object.")
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

  if (has_notes) {  # if there are notes in this table

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

  return(start_row)

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

  return(start_row)

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

  return(start_row)

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

  return(start_row)

}


# Insert sheet elements ---------------------------------------------------


.insert_title <- function(wb, content, tab_title) {

  sheet_type <- content[content$tab_title == tab_title, "sheet_type"][[1]]
  sheet_title <- content[content$tab_title == tab_title, "sheet_title"][[1]]

  if (sheet_type %in% c("cover", "contents", "notes")) {

    openxlsx::writeData(
      wb = wb,
      sheet = tab_title,
      x = sheet_title,
      startCol = 1,
      startRow = 1
    )

  }

  if (sheet_type == "tables") {

    openxlsx::writeData(
      wb = wb,
      sheet = tab_title,
      x = sheet_title,
      startCol = 1,
      startRow = 1
    )

  }

  return(wb)

}

.insert_table_count <- function(wb, content, tab_title) {

  table_count <- nrow(content[content$tab_title == tab_title, ])

  if (table_count < 10) {
    table_count <- switch(
      as.character(table_count),
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

  openxlsx::writeData(
    wb = wb,
    sheet = tab_title,
    x = text,
    startCol = 1,
    startRow = 2  # table count will always be the second row
  )

  return(wb)

}

.insert_notes_statement <- function(wb, content, tab_title) {

  has_notes <- .has_notes(content, tab_title)

  if (has_notes) {

    text <-
      "This table contains notes, which can be found in the Notes worksheet."

    openxlsx::writeData(
      wb = wb,
      sheet = tab_title,
      x = text,
      startCol = 1,
      startRow = 3  # notes will always go in row 3 if they exist
    )

  }

  return(wb)

}

.insert_blanks_message <- function(wb, content, tab_title) {

  has_blanks_message <- .has_blanks_message(content, tab_title)

  if (has_blanks_message) {

    blanks_text <- content[content$tab_title == tab_title, "blank_cells"][[1]]
    has_notes <- .has_notes(content, tab_title)
    start_row <- .get_start_row_blanks_message(has_notes)

    openxlsx::writeData(
      wb = wb,
      sheet = tab_title,
      x = blanks_text,
      startCol = 1,
      startRow = start_row
    )

  }

  return(wb)

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

      openxlsx::writeData(
        wb = wb,
        sheet = tab_title,
        x = custom_rows_text[[i]],
        startCol = 1,
        startRow = start_row + (i - 1)
      )

    }

  }

  return(wb)

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

    openxlsx::writeData(
      wb = wb,
      sheet = tab_title,
      x = source_text,
      startCol = 1,
      startRow = start_row
    )

  }

  return(wb)

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

  openxlsx::writeDataTable(
    wb = wb,
    sheet = tab_title,
    x = table,
    tableName = table_name,
    startCol = 1,
    startRow = start_row,
    tableStyle = "none",
    withFilter = FALSE,
    bandedRows = FALSE
  )

  return(wb)

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

    has_hyperlink <- .detect_hyperlink(table_with_links[[i]])

    if (has_hyperlink) {
      openxlsx::writeFormula(
        wb = wb,
        sheet = tab_title,
        x = table_with_links[[i]],
        startRow = i + 1
      )
    }

    if (!has_hyperlink) {
      openxlsx::writeData(
        wb = wb,
        sheet = tab_title,
        x = table_with_links[[i]],
        startRow = i + 1
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
    openxlsx::addWorksheet(wb, i)
  }

  return(wb)

}

.add_cover <- function(wb, content) {

  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "cover", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "cover", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_cover_table(wb, content, table_name)  # rather than .insert_table

  styles <- .style_create()
  .style_workbook(wb)
  .style_sheet_title(wb, tab_title, styles)
  .style_cover(wb, content, styles)  # TODO: needs special handling if list provided

  return(wb)

}


.add_contents <- function(wb, content) {

  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "contents", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "contents", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  .insert_table(wb, content, table_name)

  styles <- .style_create()
  .style_workbook(wb)
  .style_sheet_title(wb, tab_title, styles)
  .style_table(wb, content, table_name, styles)
  .style_contents(wb, content, styles)

  return(wb)

}


.add_notes <- function(wb, content) {

  .stop_bad_input(wb, content)

  tab_title <- content[content$sheet_type == "notes", "tab_title"][[1]]
  table_name <- content[content$sheet_type == "notes", "table_name"][[1]]

  .insert_title(wb, content, tab_title)
  .insert_table_count(wb, content, tab_title)
  .insert_custom_rows(wb, content, tab_title)
  .insert_table(wb, content, table_name)

  styles <- .style_create()
  .style_workbook(wb)
  .style_sheet_title(wb, tab_title, styles)
  .style_table(wb, content, table_name, styles)
  .style_notes(wb, content, styles)

  return(wb)

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
  .insert_table(wb, content, table_name)

  styles <- .style_create()
  .style_workbook(wb)
  .style_sheet_title(wb, tab_title, styles)
  .style_table(wb, content, table_name, styles)

  return(wb)

}
