# This file generates and writes demo datasets


# demo_df and demo_a11ytable (as of v0.3) ---------------------------------


set.seed(1066)

cover_list <- list(
  "Section 1" = c("First row of Section 1.", "Second row of Section 1."),
  "Section 2" = "The only row of Section 2.",
  "Section 3" = c(
    "[Website](https://best-practice-and-impact.github.io/a11ytables/)",
    "[Email address](mailto:fake.address@a11ytables.com)"
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
  "This column has a very long name that means that the column width needs to be widened" = 1:10,
  Notes = c("[note 2]", rep(NA_character_, 4), "[note 3]", rep(NA_character_, 4)),
  check.names = FALSE
)

table_2_df <- data.frame(Category = LETTERS[1:10], Numeric = 1:10)

demo_a11ytable <-
  a11ytables::create_a11ytable(
    tab_titles = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
    sheet_types = c("cover", "contents", "notes", "tables", "tables"),
    sheet_titles = c(
      "The 'a11ytables' Demo Workbook",
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
        "First custom row [with a hyperlink.](https://best-practice-and-impact.github.io/a11ytables/)",
        "Second custom row."
      ),
      "A custom row."
    ),
    sources = c(
      rep(NA_character_, 3),
      "[The Source Material, 2024.](https://best-practice-and-impact.github.io/a11ytables/)",
      "The Source Material, 2024."
    ),
    tables = list(cover_list, contents_df, notes_df, table_1_df, table_2_df)
  )

demo_df <- as.data.frame(demo_a11ytable)

demo_workbook <- generate_workbook(demo_a11ytable)

# Write to data/
usethis::use_data(demo_df, overwrite = TRUE)
usethis::use_data(demo_a11ytable, overwrite = TRUE)
usethis::use_data(demo_workbook, overwrite = TRUE)


# mtcars_df and mtcars_df2 (superseded in v0.3)  --------------------------


library(tibble)

cover_df <-
  tribble(
    ~"Sub title",  ~"Sub body",
    "Description", "Aspects of automobile design and performance.",
    "Properties",  paste0("Suppressed values are replaced with the value '[c]'."),
    "Contact",     "The mtcars Team, telephone 0123456789."
  ) |>
  as.data.frame()

cover_list <-
  list(
    "Description" = c(
      "Aspects of automobile design and performance from 'Motor Trend' in 1974.",
      "Used by Henderson and Velleman in a 1981 paper in 'Biometrics'."
    ),
    "Properties" =  "Suppressed values are replaced with the value '[c]'.",
    "Contact" = c(
      "[Visit the website.](https://github.com/best-practice-and-impact/a11ytables)",
      "[Email the team.](mailto:not-a-real-email-address@completely-fake.net)",
      "Telephone 0123456789"
    )
  )

contents_df <-
  tribble(
    ~"Sheet name", ~"Sheet title",
    "Notes",       "Notes used in the statistical tables of this workbook",
    "Table 1",     "Car Road Tests (demo 1)",
    "Table 2",     "Car Road Tests (demo 2)"
  ) |>
  as.data.frame()

notes_df <-
  tribble(
    ~"Note number", ~"Note text",
    "[note 1]",     "US gallons.",
    "[note 2]",     "Retained to enable comparisons with previous analyses."
  ) |>
  as.data.frame()

stats_df_1 <- mtcars |>
  head() |>
  rownames_to_column("car") |>
  subset(select = c("car", "cyl", "mpg"))

names(stats_df_1) <- c("Car", "Cylinder count", "Miles per gallon [note 1]")

stats_df_1$Notes <- c(rep("[note 2]", 2), rep(NA_character_, 4))

stats_df_1[3, 2:3] <- "[c]"

stats_df_2 <- mtcars |>
  head() |>
  rownames_to_column("car") |>
  subset(select = c("car", "hp", "drat"))

names(stats_df_2) <- c("Car", "Gross horsepower", "Rear axle ratio")

# Using cover_df as the table input for the cover
mtcars_df <- tibble(
  tab_title = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
  sheet_type = c("cover", "contents", "notes", "tables", "tables"),
  sheet_title = c(
    "The 'mtcars' Demo Dataset",
    "Table of contents",
    "Notes",
    "Table 1: Car Road Tests 1",
    "Table 2: Car Road Tests 2"
  ),
  blank_cells = c(
    rep(NA_character_, 3),
    "A blank cell in the Notes column indicates that there is no note for that row.",
    NA_character_
  ),
  source = c(rep(NA_character_, 3), rep("Motor Trend (1974)", 2)),
  table = list(cover_df, contents_df, notes_df, stats_df_1, stats_df_2)
)

# Using cover_list as the table input for the cover, introduced in v0.2
mtcars_df2 <- tibble(
  tab_title = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
  sheet_type = c("cover", "contents", "notes", "tables", "tables"),
  sheet_title = c(
    "The 'mtcars' Demo Dataset",
    "Table of contents",
    "Notes",
    "Table 1: Car Road Tests 1",
    "Table 2: Car Road Tests 2"
  ),
  blank_cells = c(
    rep(NA_character_, 3),
    "A blank cell in the Notes column indicates that there is no note for that row.",
    NA_character_
  ),
  source = c(rep(NA_character_, 3), rep("Motor Trend (1974)", 2)),
  table = list(cover_list, contents_df, notes_df, stats_df_1, stats_df_2)
)

# Write to data/
usethis::use_data(mtcars_df, overwrite = TRUE)
usethis::use_data(mtcars_df2, overwrite = TRUE)
