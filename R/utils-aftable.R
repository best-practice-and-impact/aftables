#' Clean Sheet Tab Titles
#' @param tab_titles Character vector. Names of tabs in the workbook.
#' @noRd
.check_tab_titles <- function(tab_titles) {
  tab_title_num_start <- grep("^\\d", unlist(tab_titles))

  if (length(tab_title_num_start) > 0) {
    stop(
      "Elements in tab_titles must not begin with a numeral (change ",
      .vector_to_sentence(tab_titles[tab_title_num_start]), ").",
      call. = FALSE
    )
  }
}

#' Clean Sheet Tab Titles
#' @param tab_titles Character vector. Names of tabs in the workbook.
#' @noRd
.clean_tab_titles <- function(tab_titles) {
  tab_titles_cleaned <- gsub("[^[:alnum:][:space:]_]", "", tab_titles)
  tab_titles_cleaned <- trimws(tab_titles_cleaned)
  tab_titles_cleaned <- gsub(" ", "_", tab_titles_cleaned)
  tab_titles_cleaned <- strtrim(tab_titles_cleaned, 31)

  before <- tab_titles[!tab_titles %in% tab_titles_cleaned]
  after <- tab_titles_cleaned[!tab_titles %in% tab_titles_cleaned]

  if (length(before > 0)) {
    warning(
      "These tab_titles have been cleaned automatically: ",
      paste(before, collapse = ", "),
      " (now ", paste(after, collapse = ", "), ").",
      call. = FALSE
    )
  }

  tab_titles_cleaned
}

#' Add a Missing Terminal Period to a String
#' @param text Character. A string.
#' @noRd
.append_period <- function(text) {
  last_char <- substr(text, nchar(text), nchar(text))

  for (i in seq_along(text)) {
    if (nzchar(text[i]) && !is.na(last_char[i]) && last_char[i] != ".") {
      text[i] <- paste0(text[i], ".")
    }
  }

  text
}

#' Validate an 'aftable' Object
#' @param text x. An object with class 'aftable', likely created with
#'     \code{\link{create_aftable}}.
#' @noRd
.validate_aftable <- function(x) {
  names_req <- c(
    "tab_title",
    "sheet_type",
    "sheet_title",
    "blank_cells",
    "sources",
    "custom_rows",
    "table"
  )
  names_count <- length(names_req)
  names_in <- names(x)

  # Must be of data.frame class
  if (!inherits(x, "data.frame")) {
    stop("Input must have class data.frame.", call. = FALSE)
  }

  # Must have particular dimensions (must have cover, contents table, at least)
  if (length(names_req) != length(x) || nrow(x) < 3) {
    stop(
      "Input must be a data.frame with ", names_count,
      " columns and at least 4 rows.",
      call. = FALSE
    )
  }

  # Column names must match expected format
  if (!all(names_req %in% names_in)) {
    stop(
      "Input data.frame does not have the required column names.",
      call. = FALSE
    )
  }

  # 'table' column class must be listcol
  if (!inherits(x[["table"]], "list")) {
    stop(
      "Column 'table' must be a listcol of data.frame objects.",
      call. = FALSE
    )
  }

  # 'custom_row' column class must be listcol
  if (!inherits(x[["custom_rows"]], "list")) {
    stop(
      "Column 'table' must be a listcol of character vectors",
      call. = FALSE
    )
  }

  # Class must be character for all columns except 'table', 'sources' and 'custom_rows'

  char_cols <- x[, !names(x) %in% c("table", "sources", "custom_rows")]
  are_char_cols <- unlist(lapply(char_cols, is.character))

  if (!all(are_char_cols)) {
    stop(
      "All columns except 'table' and 'custom_rows' must be character class.",
      call. = FALSE
    )
  }

  # Content of 'table' listcol must be single data.frame objects (or cover list)
  if (!all(unlist(lapply(x[["table"]], is.list)))) {
    stop(
      "List-column 'table' must contain data.frame objects only. ",
      "The cover can also be of class 'list'.",
      call. = FALSE
    )
  }

  # Content of 'sources' listcol must be character
  if (!all(unlist(lapply(x[["sources"]], is.character)))) {
    stop(
      "List-column 'sources' must contain character vectors only. ",
      call. = FALSE
    )
  }

  # Content of 'custom_row' listcol must be character
  if (!all(unlist(lapply(x[["custom_rows"]], is.character)))) {
    stop(
      "List-column 'custom_rows' must contain character vectors only. ",
      call. = FALSE
    )
  }

  # There must be cover and contents sheets
  if (sum(x[["sheet_type"]] %in% c("cover", "contents")) < 2) {
    stop(
      "The input data.frame must have sheet_type 'cover' and 'contents'.",
      call. = FALSE
    )
  }

  # There must be only one cover, contents and (optional) notes sheet
  if (
    sum(x[["sheet_type"]] == "cover") > 1 ||
      sum(x[["sheet_type"]] == "contents") > 1 ||
      sum(x[["sheet_type"]] == "notes") > 1
  ) {
    stop(
      "There can be only one 'cover', 'contents' and 'notes' in sheet_type.",
      call. = FALSE
    )
  }

  # There should be no empty rows, except in the blank_cells or source columns
  if (
    any(
      is.na(
        subset(x, select = c("tab_title", "sheet_type", "sheet_title", "table"))
      )
    )
  ) {
    stop(
      "Columns 'tab_title', 'sheet_type', 'sheet_title' and ",
      "'table' must not contain NA.",
      call. = FALSE
    )
  }

  # Each sheet_type must be only one of four types
  if (!all(x[["sheet_type"]] %in% c("cover", "contents", "notes", "tables"))) {
    stop(
      "'sheet_type' must be one of 'cover', 'contents', 'notes', 'tables'.",
      call. = FALSE
    )
  }

  # Each tab_title must be unique
  if (length(x[["tab_title"]]) != length(unique(tolower(x[["tab_title"]])))) {
    stop("Each 'tab_title' must be unique (case-insensitive).", call. = FALSE)
  }
}

#' Warn if an 'aftable' Has a Non-critical Problem
#' @param text x. An object with class 'aftable', likely created with
#'     \code{\link{create_aftable}}.
#' @noRd
.warn_aftable <- function(content) {
  # Warn about tab_title limitations

  tab_titles <- content$tab_title

  if (any(nchar(tab_titles) > 31)) {
    warning(
      "Each tab_title must be shorter than 31 characters.",
      call. = FALSE
    )
  }

  if (any(grepl("[^[:alnum:]_]", tab_titles))) {
    warning(
      "Each tab_title must contain only letters, numbers or underscores.",
      call. = FALSE
    )
  }

  # Warn if provide blank_cells on non-table sheet

  non_table_blank_cells <- content[content$sheet_type != "tables", "blank_cells"]

  if (!all(is.na(non_table_blank_cells))) {
    warning(
      "You have provided a blank_cells for a sheet which doesn't have types 'tables'. This will be removed.",
      call. = FALSE
    )
  }


  # Warn about missing sources

  non_table_sources <- content[content$sheet_type != "tables", "sources"]

  if (!all(is.na(non_table_sources))) {
    warning(
      "You have provided a source for a sheet which doesn't have types 'tables'. This will be removed.",
      call. = FALSE
    )
  }

  table_sources <- content[content$sheet_type == "tables", "sources"]

  if (
    any(
      purrr::map_lgl(
        table_sources, .f = \(x) length(x) == 1 && (is.na(x) || !nzchar(x))
      )
    )
  ) {
    warning(
      "One of your tables is missing a source statement.",
      call. = FALSE
    )
  }

  if (
    any(
      purrr::map_lgl(
        table_sources,
        .f = \(x) length(x) > 1 && any(is.na(x) | !nzchar(x))
      )
    )
  ) {
    warning(
      "One of your source statements is blank (NA or empty string) and will be removed.",
      call. = FALSE
    )
  }

  # Warn about NA rows in custom cols

  cover_custom_rows <- unlist(content[content$sheet_type == "cover", "custom_rows"])

  if (!all(is.na(cover_custom_rows))) {
    warning(
      "You have provided a custom row for the cover sheet. This will be removed.",
      call. = FALSE
    )
  }

  custom_rows <- content[content$sheet_type != "cover", "custom_rows"]

  if (
    any(
      purrr::map_lgl(
        custom_rows,
        .f = \(x) any(!nzchar(x)) || (length(x) > 1 && any(is.na(x)))
      )
    )
  ) {
    warning(
      "One of your custom rows is blank (NA or empty string) and will be removed.",
      call. = FALSE
    )
  }


  # Warn about possibly missing rows in the contents table

  contents_table <- content[content$sheet_type == "contents", "table"][[1]]

  if (nrow(content) != nrow(contents_table) + 2) {
    warning(
      "There are ", nrow(content) - 2, " tables but ",
      nrow(contents_table), " in the contents sheet.",
      call. = FALSE
    )
  }

  # Warn about notes (missing notes sheet, or notes in table)

  tables_sheets <- content[content$sheet_type == "tables", ]

  has_notes_sheet <- ifelse(
    nrow(content[content$sheet_type == "notes", ]) > 0,
    TRUE, FALSE
  )

  has_notes <-
    any(
      unlist(
        lapply(
          tables_sheets[, "tab_title"],
          function(x) .has_notes(content, x)
        )
      )
    )


  if (has_notes_sheet) {
    if (!has_notes) {
      warning(
        "You have a 'notes' sheet, but no notes in your tables.",
        call. = FALSE
      )
    }
  }

  if (has_notes) {
    if (!has_notes_sheet) {
      warning(
        "You have notes in your tables, but no 'notes' sheet.",
        call. = FALSE
      )
    }
  }

  # Warn about notes (note sheet present, but mismatches exist)

  if (has_notes_sheet) {
    notes_sheet <- content[content$sheet_type == "notes", ] # max of one notes sheet

    notes_sheet_vector <- notes_sheet[, "table"][[1]][[1]] # assumes first col has note values

    notes_sheet_values <- unlist( # e.g. get c(1, 2) from c("[note 1]", "[note 2]")
      regmatches(
        notes_sheet_vector,
        gregexpr("\\d", notes_sheet_vector, perl = TRUE) # extract numbers
      )
    )

    tables_sheet_notes <- sort(
      unique(
        unlist(
          lapply(
            tables_sheets$tab_title,
            function(x) .extract_note_values(content, x)
          )
        )
      )
    )

    not_in_tables <- setdiff(notes_sheet_values, tables_sheet_notes)
    not_in_notes <- setdiff(tables_sheet_notes, notes_sheet_values)

    if (has_notes_sheet && has_notes && length(not_in_notes) > 0) {
      warning(
        "Some notes are in the tables (",
        paste(not_in_notes, collapse = ", "),
        ") but are missing from the notes sheet.",
        call. = FALSE
      )
    }

    if (has_notes_sheet && has_notes && length(not_in_tables) > 0) {
      warning(
        "Some notes are in the notes sheet (",
        paste(not_in_tables, collapse = ", "),
        ") but are missing from the tables.",
        call. = FALSE
      )
    }
  }

  # Warn about blank cells in tables

  tables_list <- stats::setNames(
    tables_sheets[["table"]], tables_sheets[["tab_title"]]
  )

  tables_with_na_lgl <- unlist(
    lapply(
      tables_list,
      function(x) any(!stats::complete.cases(x))
    )
  )

  tables_with_na_names <- names(tables_with_na_lgl[tables_with_na_lgl])

  tables_with_blanks_reason <-
    tables_sheets[!is.na(tables_sheets$blank_cells), ][["tab_title"]]

  tables_with_na_no_reason <-
    setdiff(tables_with_na_names, tables_with_blanks_reason)

  tables_with_reason_no_na <-
    setdiff(tables_with_blanks_reason, tables_with_na_names)

  if (length(tables_with_na_no_reason) > 0) {
    warning(
      "You have blank cells in these tables but haven't provided a reason: ",
      paste(tables_with_na_no_reason, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (length(tables_with_reason_no_na) > 0) {
    warning(
      paste(
        "There's no blank cells in these tables,",
        "but you've provided a reason for blank cells: "
      ),
      paste(tables_with_reason_no_na, collapse = ", "), ".",
      call. = FALSE
    )
  }
}


#' Clean aftable to remove metadata which isn't needed
#' @param text x. An object with class 'aftable', likely created with
#'     \code{\link{create_aftable}}.
#' @noRd
.clean_aftable <- function(x) {

  # Remove blanks message from non-table sheets

  x[["blank_cells"]][x[["sheet_type"]] != "tables" | !nzchar(x[["blank_cells"]])] <- NA


  # Remove blank sources

  x[["sources"]] <- purrr::map2(
    x[["sources"]],
    x[["sheet_type"]],
    .f = \(val, sheet_type) {

      val <- val[!is.na(val) & nzchar(val)]

      if (sheet_type != "tables" || length(val) == 0) {
        val <- NA_character_
      }

      val

    }
  )


  # Remove blank custom rows

  x[["custom_rows"]] <- purrr::map2(
    x[["custom_rows"]],
    x[["sheet_type"]],
    .f = \(val, sheet_type) {

      val <- val[!is.na(val) & nzchar(val)]

      if (sheet_type == "cover" || length(val) == 0) {
        val <- NA_character_
      }

      val


    }
  )


  x

}
