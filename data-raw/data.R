# This file generates and writes demo datasets


# demo_df and demo_aftable (as of v0.3) ---------------------------------


set.seed(1066)

cover_list <- list(
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
  "Note number" = paste0("[note ", 1:3, "]"),
  "Note text" = c("First note.", "Second note.", "Third note."),
  check.names = FALSE
)

table_1_df <- data.frame(
  Category = LETTERS[1:10],
  "Numeric [note 1]" = 1:10,
  "Numeric suppressed" = c(1:4, "[c]", 6:9, "[x]"),
  "Numeric thousands" = abs(round(rnorm(10), 4) * 1e5),
  "Numeric decimal" = abs(round(rnorm(10), 5)),
  "Long name that means that the column width needs to be widened" = 1:10,
  Notes = c("[note 2]", rep(NA_character_, 4), "[note 3]", rep(NA_character_, 4)),
  check.names = FALSE
)

table_2_df <- data.frame(Category = LETTERS[1:10], Numeric = 1:10)

demo_aftable <- create_aftable(
  tab_titles = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
  sheet_types = c("cover", "contents", "notes", "tables", "tables"),
  sheet_titles = c(
    "The 'aftables' Demo Workbook",
    "Table of contents",
    "Notes",
    "Table_1: First Example Sheet",
    "Table_2: Second Example Sheet"
  ),
  blank_cells = c(
    rep(NA_character_, 3),
    "Blank cells indicate that there's no note in that row.",
    NA_character_
  ),
  custom_rows = list(
    NA_character_,
    NA_character_,
    "A custom row.",
    c(
      "First custom row [with a hyperlink.](https://best-practice-and-impact.github.io/aftables/)",
      "Second custom row."
    ),
    "A custom row."
  ),
  sources = c(
    rep(NA_character_, 3),
    "[The Source Material, 2024.](https://best-practice-and-impact.github.io/aftables/)",
    "The Source Material, 2024."
  ),
  tables = list(cover_list, contents_df, notes_df, table_1_df, table_2_df)
)

demo_df <- as.data.frame(demo_aftable)

demo_workbook <- generate_workbook(demo_aftable)

# Write to data/
usethis::use_data(demo_df, overwrite = TRUE)
usethis::use_data(demo_aftable, overwrite = TRUE)
usethis::use_data(demo_workbook, overwrite = TRUE)


