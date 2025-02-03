
#' Create An 'aftable' Object
#'
#' Create a new aftable-class object, which is a special data.frame that
#' contains all the information needed in your output spreadsheet. In turn, the
#' object created by this function can be used to populate an 'openxlsx'
#' Workbook-class object with the function \code{\link{generate_workbook}}.
#'
#' @param tab_titles Required character vector, one value per sheet. Each title
#'     will appear literally on each tab of the final spreadsheet output. Keep
#'     brief. Letters and numbers only; do not start with a number; use
#'     underscores for spaces. For example: 'Cover', 'Contents', 'Notes',
#'     'Table_1'. Will be corrected automatically unless there's an error.
#' @param sheet_types Required character vector, one value per sheet. Sheets
#'     that don't contain publication tables ('meta' sheets) should be of type
#'     'contents', 'cover' or 'notes'. Sheets that contain statistical tables of
#'     data are type 'tables'.
#' @param sheet_titles Required character vector, one value per sheet. The main
#'     title for each sheet, which will appear in cell A1 (top-left corner).
#' @param blank_cells Optional character vector, one value per sheet. A short
#'     sentence to explain the reason for any blank cells in the sheet. Supply
#'     as \code{NA_character_} if empty. Most likely to be used with sheet type
#'     'tables'.
#' @param custom_rows Optional list of character vectors. One list element per
#'     sheet, one character vector element per row of pre-table metadata. Supply
#'     a list element as \code{NA_character_} if empty. To be used with sheet
#'     type 'tables', but can also be used for sheet types 'contents' and
#'     'notes'.
#' @param sources Optional character vector, one value per sheet. The origin of
#'     the data for a given sheet. Supply as \code{NA_character_} if empty. To
#'     be used with sheet type 'tables'.
#' @param tables Required list of data.frames (though the cover sheet may be
#'     supplied as a list), one per sheet. See details.
#'
#' @details
#'
#' ## How to supply data to the 'tables' argument
#'
#' Formats for the elements collected as a list and passed to the 'tables'
#' argument, depending on the sheet type.
#'
#' \itemize{
#'     \item Sheet type 'cover': either (a) a list where each element name is a
#'         section header and each element's content is a character vector
#'         whose elements will make up separate rows of that section
#'         (recommended), or (b) a data.frame with one row per subsection, with
#'         one column for section titles and one column for corresponding for
#'         that section's body text. For example, you may have a section with
#'         the title 'Contact details' that contains an email address and
#'         telephone number. You can use linebreaks (i.e. '\\n') to separate
#'         text into paragraphs.
#'     \item Sheet type 'contents': one row per sheet, two columns suggested at
#'         least (named 'Tab title' and 'Worksheet title').
#'     \item Sheet type 'notes': one row per note, two columns suggested (named
#'         'Note number', 'Note text'), where notes are in the form
#'         '\[note 1\]'.
#'     \item Sheet type 'tables': a tidy, rectangular data.frame containing the
#'         data to be published. It's the user's responsibility to add notes in
#'         the form '\[note 1\]' to column headers, or in a special 'Notes' row.
#' }
#'
#' ## How to supply hyperlinks
#'
#' You can provide text in Markdown link syntax (e.g.
#' '\[GOV.UK\](https://www.gov.uk)', adding 'mailto:' before an
#' email address) and the containing cell will be rendered as a hyperlink in the
#' output spreadsheet. Note that whole cells will become hyperlinks; there is no
#' support for selected words in a sentence to be rendered as a hyperlink.
#'
#' Hyperlinks can be supplied in the character strings to three arguments:
#'
#' \itemize{
#'     \item To the 'tables' argument for sheet type 'cover' only. It's
#'         recommended to supply the cover information as a list rather than a
#'         data.frame, which will allow you to make specific rows within a
#'         section (e.g. 'contact us') into hyperlinks.
#'     \item To the 'custom_rows' argument for sheets of type 'contents, 'notes'
#'        and 'tables'.
#'     \item To the 'source' argument for sheets of type 'table' only.
#' }
#'
#' @return An object with classes 'aftable', 'tbl' and 'data.frame'.
#'
#' @examples
#' # Prepare some demo tables of information
#'
#' set.seed(1066)
#'
#' cover_list <- list(
#'   "Section 1" = c("First row of Section 1.", "Second row of Section 1."),
#'   "Section 2" = "The only row of Section 2.",
#'   "Section 3" = c(
#'     "[Website](https://best-practice-and-impact.github.io/aftables/)",
#'     "[Email address](mailto:fake.address@aftables.com)"
#'   )
#' )
#'
#' contents_df <- data.frame(
#'   "Sheet name" = c("Notes", "Table_1", "Table_2"),
#'   "Sheet title" = c(
#'     "Notes used in this workbook",
#'     "First Example Sheet",
#'     "Second Example Sheet"
#'   ),
#'   check.names = FALSE
#' )
#'
#' notes_df <- data.frame(
#'   "Note number" = paste0("[note ", 1:3, "]"),
#'   "Note text" = c("First note.", "Second note.", "Third note."),
#'   check.names = FALSE
#' )
#'
#' table_1_df <- data.frame(
#'   Category = LETTERS[1:10],
#'   "Numeric [note 1]" = 1:10,
#'   "Numeric suppressed" = c(1:4, "[c]", 6:9, "[x]"),
#'   "Numeric thousands" = abs(round(rnorm(10), 4) * 1e5),
#'   "Numeric decimal" = abs(round(rnorm(10), 5)),
#'   "This column has a very long name that means that the column width needs to be widened" = 1:10,
#'   Notes = c("[note 1]", rep(NA_character_, 4), "[note 2]", rep(NA_character_, 4)),
#'   check.names = FALSE
#' )
#'
#' table_2_df <- data.frame(Category = LETTERS[1:10], Numeric = 1:10)
#'
#' # Create 'aftables' object
#'
#' x <-
#'   aftables::create_aftable(
#'     tab_titles = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
#'     sheet_types = c("cover", "contents", "notes", "tables", "tables"),
#'     sheet_titles = c(
#'       "The 'aftables' Demo Workbook",
#'       "Table of contents",
#'       "Notes",
#'       "Table 1: First Example Sheet",
#'       "Table 2: Second Example Sheet"
#'     ),
#'     blank_cells = c(
#'       rep(NA_character_, 3),
#'       "Blank cells indicate that there's no note in that row.",
#'       NA_character_
#'     ),
#'     custom_rows = list(
#'       NA_character_,
#'       NA_character_,
#'       "A custom row.",
#'       c(
#'         "First custom row [with a hyperlink.](https://best-practice-and-impact.github.io/aftables/)",
#'         "Second custom row."
#'       ),
#'       "A custom row."
#'     ),
#'     sources = c(
#'       rep(NA_character_, 3),
#'       "[The Source Material, 2024.](https://best-practice-and-impact.github.io/aftables/)",
#'       "The Source Material, 2024."
#'     ),
#'     tables = list(cover_list, contents_df, notes_df, table_1_df, table_2_df)
#'   )
#'
#' # Test that 'aftable' is one of the object's classes
#' is_aftable(x)
#'
#' # Look at the structure of the object
#' str(x, max.level = 2)
#'
#' @export
create_aftable <- function(tab_titles,
                           sheet_types = c("cover", "contents", "notes", "tables"),
                           sheet_titles,
                           blank_cells = NA_character_,
                           sources = NA_character_,
                           custom_rows = list(NA_character_),
                           tables) {

  x <- data.frame(
    tab_title   = unlist(tab_titles),
    sheet_type  = unlist(sheet_types),
    sheet_title = unlist(sheet_titles),
    blank_cells = unlist(blank_cells),
    source      = unlist(sources),
    stringsAsFactors = FALSE  # because default is TRUE prior to R v4
  )

  x[["custom_rows"]] <- custom_rows
  x[["table"]] <- tables

  as_aftable(x)

}

#' Coerce To An 'aftable' Object
#'
#' Functions to check if an object is an aftable, or coerce it if possible.
#'
#' @param x A data.frame object to coerce.
#'
#' @return \code{as_aftable} returns an object of class aftable if possible.
#'     \code{is_aftable} returns \code{TRUE} if the object has class
#'     aftable, otherwise \code{FALSE}.
#'
#' @examples
#' is_aftable(demo_aftable)
#'
#' @export
as_aftable <- function(x) {

  if (any(names(x) %in% "tab_title")) {
    .check_tab_titles(x[["tab_title"]])
    x[["tab_title"]] <- .clean_tab_titles(x[["tab_title"]])
  }

  if (any(names(x) %in% "blank_cells")) {
    x[["blank_cells"]] <- .append_period(x[["blank_cells"]])
  }

  class(x) <- c("aftable", "tbl", "data.frame")

  .validate_aftable(x)
  .warn_aftable(x)

  x

}

#' @rdname as_aftable
#' @export
is_aftable <- function(x) {

  inherits(x, "aftable")

}

#' Summarise An 'aftable' Object
#'
#' A concise result summary of an aftable-class object to see information
#' about the sheet content. Shows a numbered list of sheets with each tab title,
#' sheet type and table dimensions.
#'
#' @param object An aftable-class object for which to get a summary.
#' @param ... Other arguments to pass.
#'
#' @examples
#' # Print a concise summary of the aftable-class object
#' summary(demo_aftable)
#'
#' # Alternatively, look at the structure
#' str(demo_aftable, max.level = 2)
#'
#' @export
summary.aftable <- function(object, ...) {

  tables <- object[["table"]]

  table_dims <- vector("list", length = length(tables))

  for (i in seq_along(tables)) {

    if (inherits(tables[[i]], "list")) {

      list_length  <- length(tables[[i]])
      list_lengths <- lengths(tables[[i]])

      table_dims[[i]] <- paste0(
        "list of length ", list_length,
        " (element lengths ", .vector_to_sentence(list_lengths), ")"
      )

    }

    if (is.data.frame(tables[[i]])) {
      table_dims[[i]] <-
        paste(paste(dim(tables[[i]]), collapse = " x "), "dataframe")
    }

  }

  summary_string <- paste0(
    "\n",
    paste0(
      paste0("  ", seq_along(tables), ") Tab '"),
      object[["tab_title"]],
      "' (sheet type '", object[["sheet_type"]], "') contains a ",
      unlist(table_dims),
      collapse = "\n"
    )
  )

  cat("# An aftable with", nrow(object), "sheets:", summary_string)

  invisible(object)

}


#' Provide A Succinct Summary Of An 'aftable' Object
#'
#' A brief text description of an aftable-class object.
#'
#' @param x An aftable-class object to summarise.
#' @param ... Other arguments to pass.
#'
#' @return Named character vector.
#'
#' @examples
#' # Print with description
#' print(demo_aftable)
#'
#' # Print description only (package 'tibble' must be installed)
#' tibble::tbl_sum(demo_aftable)
#'
#' @export
tbl_sum.aftable <- function(x, ...) {

  header <- sprintf(
    "%s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )

  c("aftable" = header)

}
