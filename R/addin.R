#' Insert Demo 'create_aftable' Template
#'
#' Insert at the cursor a template for \code{\link{create_aftable}} from the
#' 'aftable' package, pre-filled with demo data.
#'
#' @export
at_template_aftable <- function() {
  rstudioapi::insertText(string_create_aftable())
}

#' Insert Full Demo 'aftables' Template Workflow
#'
#' Insert at the cursor (a) demo templates for cover, contents and notes
#' tables, and (b) a call to \code{\link{create_aftable}} pre-filled with
#' demo data.
#'
#' @export
at_template_workflow <- function() {

  rstudioapi::insertText(
    paste0(
      "# Prepare tables of information",
      "\n\n",
      string_tables(),
      "\n\n",
      "# Create new aftable",
      "\n\n",
      string_create_aftable(),
      "\n\n",
      "# Generate workbook from aftable",
      "\n\n",
      "my_wb <- aftables::generate_workbook(my_aftable)",
      "\n\n",
      "# Create output",
      "\n\n",
      "openxlsx::openXL(my_wb)  # open temp copy",
      "\n\n",
      'openxlsx::saveWorkbook(my_wb, "example.xlsx")  # change save location'
    )
  )

}

#' A String Containing Code to Prepare Tables for an 'aftables' Object
#' @noRd
string_tables <- function() {

  'cover_list <- list(
  "Section 1" = c("First row of Section 1.", "Second row of Section 1."),
  "Section 2" = "The only row of Section 2.",
  "Section 3" = c(
    "[Website](https://best-practice-and-impact.github.io/aftables/)",
    "[Email address](mailto:fake.address@aftables.com)"
  )
)

contents_df <- data.frame(
  "Sheet name" = c("Notes", "Table 1", "Table 2"),
  "Sheet title" = c(
    "Notes used in this workbook",
    "First Example Sheet",
    "Second Example Sheet"
  ),
  check.names = FALSE
)

notes_df <- data.frame(
  "Note number" = paste("[note ", 1:2, "]"),
  "Note text" = c("First note.", "Second note."),
  check.names = FALSE
)

table_1_df <- data.frame(
  Category = LETTERS[1:10],
  Numeric = 1:10,
  "Numeric suppressed" = c(1:4, "[c]", 6:9, "[x]"),
  "Numeric thousands" = abs(round(rnorm(10), 4) * 1e5),
  "Numeric decimal" = abs(round(rnorm(10), 5)),
  "A column with a long name" = 1:10,
  Notes = c("[note 1]", rep(NA_character_, 4), "[note 2]", rep(NA_character_, 4)),
  check.names = FALSE
)

table_2_df <- data.frame(Category = LETTERS[1:10], Numeric = 1:10)'

}

#' A String Containing Code to Generate an 'aftables' Object
#' @noRd
string_create_aftable <- function() {

  'my_aftable <-
  aftables::create_aftable(
    tab_titles = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
    sheet_types = c("cover", "contents", "notes", "tables", "tables"),
    sheet_titles = c(
      "The \'aftables\' Demo Workbook",
      "Table of contents",
      "Notes",
      "Table 1: First Example Sheet",
      "Table 2: Second Example Sheet"
    ),
    blank_cells = c(
      rep(NA_character_, 3),
      "Blank cells indicate that there\'s no note in that row.",
      NA_character_
    ),
    custom_rows = list(
      NA_character_,
      "A custom row in the Contents sheet.",
      NA_character_,
      c(
        "First custom row for Table 1.",
        "A second custom row [with a hyperlink.](https://best-practice-and-impact.github.io/aftables/)"
      ),
      "A custom row for Table 2"
    ),
    sources = c(
      rep(NA_character_, 3),
      "[The Source Material, 2024](https://best-practice-and-impact.github.io/aftables/)",
      "The Source Material, 2024"
    ),
    tables = list(cover_list, contents_df, notes_df, table_1_df, table_2_df)
  )'

}
