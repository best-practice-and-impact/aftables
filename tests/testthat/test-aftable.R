test_that("aftable can be created by hand (with list for cover)", {
  # Uses demo_df, which has a list containing cover information in the
  # 'table' column.
  expect_warning(
    expect_warning(
      test_aftable <-
        create_aftable(
          tab_titles = demo_df$tab_title,
          sheet_types = demo_df$sheet_type,
          sheet_titles = demo_df$sheet_title,
          tables = demo_df$table
        ),
      "One of your tables is missing a source statement."
    ),
    "You have blank cells in these tables but haven't provided a reason: Table_1."
  )

  expect_s3_class(test_aftable, class = "aftable")
  expect_identical(class(test_aftable), c("aftable", "tbl", "data.frame"))

  expect_error(
    create_aftable(
      tab_titles   = demo_df$tab_title,
      sheet_types  = "x",
      sheet_titles = demo_df$sheet_title,
      tables       = demo_df$table
    ),
    "The input data.frame must have sheet_type 'cover' and 'contents'."
  )

})

test_that("aftable can be created by hand (with df for cover)", {

  expect_warning(
    expect_warning(
      test_aftable <-
        create_aftable(
          tab_titles = demo_df$tab_title,
          sheet_types = demo_df$sheet_type,
          sheet_titles = demo_df$sheet_title,
          tables = c(
            list(data.frame("Section 1" = "Text.", "Section 2" = "Text.")),
            demo_df$table[2:length(demo_df$table)]
          )
        ),
      "One of your tables is missing a source statement."
    ),
    "You have blank cells in these tables but haven't provided a reason: Table_1."
  )

  expect_s3_class(test_aftable, class = "aftable")
  expect_identical(class(test_aftable), c("aftable", "tbl", "data.frame"))

  expect_error(
    expect_warning(
      create_aftable(
        tab_titles   = demo_df$tab_title,
        sheet_types  = "x",
        sheet_titles = demo_df$sheet_title,
        tables       = demo_df$table
      )
    ),
    "The input data.frame must have sheet_type 'cover' and 'contents'."
  )

})

test_that("strings are not converted to factors", {

  expect_warning(
    expect_warning(
      test_aftable <-
        create_aftable(
          tab_titles = demo_df$tab_title,
          sheet_types = demo_df$sheet_type,
          sheet_titles = demo_df$sheet_title,
          tables = demo_df$table
        ),
      "One of your tables is missing a source statement."
    ),
    "You have blank cells in these tables but haven't provided a reason: Table_1."
  )

  classes <- unlist(lapply(test_aftable, class))

  expect_true(all(c("character", "list") %in% classes))
  expect_false(any("factor" %in% classes))
})

test_that("suitable objects can be coerced", {

  test_aftable <-
    as_aftable(demo_df)

  expect_s3_class(test_aftable, class = "aftable")
  expect_identical(class(test_aftable), c("aftable", "tbl", "data.frame"))

  expect_identical(is_aftable(test_aftable), TRUE)
  expect_identical(is_aftable("test_aftable"), FALSE)

  expect_true(is_aftable(test_aftable))
  expect_false(is_aftable(mtcars))
})

test_that("class validation works", {

  expect_length(
    as_aftable(demo_df),
    7
  )

  expect_error(as_aftable(1),
               "Input must be a data.frame with 7 columns and at least 4 rows.")
  expect_error(as_aftable("x"),
               "Input must be a data.frame with 7 columns and at least 4 rows.")
  expect_error(as_aftable(list()),
               "Input must be a data.frame with 7 columns and at least 4 rows.")
  expect_error(as_aftable(data.frame()),
               "Input must be a data.frame with 7 columns and at least 4 rows.")

  test_demo_df <- demo_df
  names(test_demo_df)[1] <- "foo"
  expect_error(as_aftable(test_demo_df),
               "Input data.frame does not have the required column names.")

  test_demo_df <- demo_df
  test_demo_df[["table"]] <- as.character(test_demo_df[["table"]])
  expect_error(as_aftable(test_demo_df),
               "Column 'table' must be a listcol of data.frame objects.")

  test_demo_df <- demo_df[, 1:4]
  expect_error(as_aftable(test_demo_df),
               "Input must be a data.frame with 7 columns and at least 4 rows.")

  test_demo_df <- demo_df[1, ]
  expect_error(as_aftable(test_demo_df),
               "Input must be a data.frame with 7 columns and at least 4 rows.")

  test_demo_df <- demo_df
  test_demo_df[test_demo_df$sheet_type %in% c("cover", "contents"), "sheet_type"] <- "foo"
  expect_error(as_aftable(test_demo_df),
               "The input data.frame must have sheet_type 'cover' and 'contents'.")

  test_demo_df <- demo_df
  test_demo_df[test_demo_df$tab_title == "Table_2", "sheet_type"] <- "foo"
  expect_error(as_aftable(test_demo_df),
               "'sheet_type' must be one of 'cover', 'contents', 'notes', 'tables'.")

  test_demo_df <- demo_df
  test_demo_df$sheet_type <- NA_character_
  expect_error(as_aftable(test_demo_df),
               "The input data.frame must have sheet_type 'cover' and 'contents'.")

  test_demo_df <- demo_df
  test_demo_df$custom_rows <- NA_character_
  expect_error(as_aftable(test_demo_df),
               "Column 'table' must be a listcol of character vectors")

  test_demo_df <- demo_df
  test_demo_df$custom_rows <- rep(list(1), nrow(test_demo_df))
  expect_error(as_aftable(test_demo_df),
               "List-column 'custom_rows' must contain character vectors only.")

  test_demo_df <- demo_df
  test_demo_df[test_demo_df$tab_title == "Table_2", "tab_title"] <-
    "Lorem_ipsum_dolor_sit_amet__consectetur_adipiscing"

  expect_warning(
    as_aftable(test_demo_df),
    "These tab_titles have been cleaned automatically:"
  )

  test_demo_df <- demo_df
  test_demo_df[test_demo_df$tab_title == "Table_2", "tab_title"] <- "!?"

  expect_warning(
    as_aftable(test_demo_df),
    "These tab_titles have been cleaned automatically:"
  )

  test_demo_df <- demo_df
  test_demo_df[test_demo_df$sheet_type == "notes", "table"][[1]] <-
    list(
      data.frame(
        "Note number" = "[note 1]",
        "Note text" = "US gallons.",
        check.names = FALSE
      )
    )

  expect_warning(
    as_aftable(test_demo_df),
    "Some notes are in the tables"
  )

})

test_that("summary method works", {
  test_demo_df <- suppressWarnings(as_aftable(demo_df))
  expect_output(summary(test_demo_df),
                "An aftable with 5 sheets:")
})

test_that("absence of note sheets doesn't prevent aftable formation", {
  test_demo_df <- demo_df[demo_df$sheet_type != "notes", ]

  expect_warning(
    expect_warning(
      test_aftable <- as_aftable(test_demo_df),
      "There are 2 tables but 3 in the contents sheet."
    ),
    "You have notes in your tables, but no 'notes' sheet."
  )

  expect_s3_class(test_aftable, "aftable")

  expect_warning(
    test_wb <- generate_workbook(test_aftable),
    "Some of the recommended workbook properties are missing."
  )

  expect_s3_class(test_wb, c("wbWorkbook", "R6"))
})

test_that("tab_titles with starting numeral will error", {
  demo_df[demo_df$tab_title == "Table_2", "tab_title"] <- "2_Table"

  expect_error(
    with(
      demo_df,
      create_aftable(
        tab_titles   = tab_title,
        sheet_types  = sheet_type,
        sheet_titles = sheet_title,
        tables       = table
      )
    ),
    "Elements in tab_titles must not begin with a numeral \\(change 2_Table\\)\\."
  )
})

test_that("tbl output looks as intended", {
  x <- create_aftable(
    tab_titles = LETTERS[1:3],
    sheet_types = c("cover", "contents", "tables"),
    sheet_titles = LETTERS[1:3],
    sources = c(NA_character_, NA_character_, "Source"),
    tables = list(
      data.frame(x = "x"),
      data.frame(tab = "x", title = "x"),
      mtcars
    )
  )

  expect_snapshot_output(as_aftable(x))
})

test_that("non-conforming tab_titles are cleaned", {
  expect_warning(
    create_aftable(
      tab_titles = c("cover", "contents", "Table 2"),
      sheet_types = c("cover", "contents", "tables"),
      sheet_titles = c("Cover", "Contents", "Table"),
      sources = "Source",
      tables = list(
        demo_df[["table"]][[1]],
        demo_df[["table"]][[2]][3, ],
        mtcars
      )
    ),
    "These tab_titles have been cleaned automatically: Table 2 \\(now Table_2\\)"
  )
})

test_that("tab_titles are unique", {
  demo_df[demo_df$tab_title == "Table_2", "tab_title"] <- "Table_1"

  expect_error(
    with(
      demo_df,
      create_aftable(
        tab_titles   = tab_title,
        sheet_types  = sheet_type,
        sheet_titles = sheet_title,
        tables       = table
      )
    )
  )
})

test_that("input other than data.frame is intercepted during validation", {
  expect_error(.validate_aftable("x"),
               "Input must have class data.frame.")

  test_demo_df <- demo_df
  test_demo_df[, "table"] <- "x"
  expect_error(
    as_aftable(test_demo_df),
    "Column 'table' must be a listcol of data.frame objects."
  )

  y <- subset(demo_df, select = -table)
  y[, "table"] <- list(rep(list("x"), nrow(y)))
  expect_error(
    as_aftable(y),
    "List-column 'table' must contain data.frame objects only."
  )
})

test_that("only one cover, contents, notes can be used", {
  cover_dupe <-
    rbind(demo_df, demo_df[demo_df$sheet_type == "cover", ])
  contents_dupe <-
    rbind(demo_df, demo_df[demo_df$sheet_type == "contents", ])
  notes_dupe <-
    rbind(demo_df, demo_df[demo_df$sheet_type == "notes", ])

  expect_error(as_aftable(cover_dupe))
  expect_error(as_aftable(contents_dupe))
  expect_error(as_aftable(notes_dupe))
})

test_that("NAs in certain columns cause failure", {
  tab_na <- demo_df
  tab_na$tab_title <- NA_character_
  type_na <- demo_df
  type_na$sheet_type <- NA_character_
  title_na <- demo_df
  title_na$sheet_title <- NA_character_
  table_na <- demo_df
  table_na$table <- NA_character_

  expect_error(as_aftable(tab_na))
  expect_error(as_aftable(type_na))
  expect_error(as_aftable(title_na))
  expect_error(as_aftable(table_na))
})

test_that("Note mismatch is caught", {
  test_demo_df <- demo_df[!demo_df$tab_title == "Table_1", ]
  test_demo_df[test_demo_df$sheet_type == "contents", "table"][[1]] <-
    list(data.frame(x = c("x", "y"), y = c("x", "y")))

  as_aftable(demo_df)

  test_demo_df <- demo_df[!demo_df$tab_title == "Table_2", ]
  test_demo_df[test_demo_df$sheet_type == "contents", "table"][[1]] <-
    list(data.frame(x = c("x", "y"), y = c("x", "y")))
  test_demo_df[test_demo_df$sheet_type == "notes", "table"][[1]] <- list(
    data.frame(
      `Note number` = paste0("[note ", 1:4, "]"),
      `Note text` = "x",
      check.names = FALSE
    )
  )

  expect_warning(
    as_aftable(test_demo_df),
    "Some notes are in the notes sheet"
  )
})


test_that("warning is raised if a source statement is missing", {
  demo_df[demo_df$tab_title == "Table_1", "source"] <- NA_character_

  expect_warning(
    as_aftable(demo_df),
    "One of your tables is missing a source statement."
  )
})

test_that("warning is raised if there's no blank cells but there is a reason", {
  demo_df[demo_df$tab_title == "Table_2", "blank_cells"] <- "x"

  expect_warning(
    as_aftable(demo_df),
    "There's no blank cells in these tables"
  )
})

test_that("period added to end of text if needed", {
  expect_identical(.append_period("Test"), "Test.")
  expect_identical(.append_period("Test."), "Test.")
  expect_identical(.append_period(NA), NA)

  expect_identical(
    .append_period(c("Test", "Test.", NA_character_)),
    c("Test.", "Test.", NA)
  )
})

test_that("tab titles are cleaned and warnings provided", {
  long_title <- "X12345678901234567890123456789012"

  expect_warning(
    .clean_tab_titles("Table 1"),
    "These tab_titles have been cleaned automatically: Table 1 \\(now Table_1\\)\\."
  )

  expect_warning(
    .clean_tab_titles(c("Table 1", "Table 2")),
    "These tab_titles have been cleaned automatically: Table 1, Table 2 \\(now Table_1, Table_2\\)\\."
  )

  expect_warning(
    .clean_tab_titles(c("Table_1", "Table 2")),
    "These tab_titles have been cleaned automatically: Table 2 \\(now Table_2\\)\\."
  )

  expect_warning(
    .clean_tab_titles("Table!@£#$%^&*(){}[]-=+;:'\"\\|<>,.~`/?1"),
    "These tab_titles have been cleaned automatically:.+now Table1\\)\\."
  )

  expect_warning(
    .clean_tab_titles(long_title),
    "These tab_titles have been cleaned automatically: X12345678901234567890123456789012 \\(now X123456789012345678901234567890\\)\\."
  )

  test_demo_df <- demo_df
  test_demo_df[1, "tab_title"] <- long_title

  expect_warning(
    as_aftable(test_demo_df),
    "These tab_titles have been cleaned automatically: X12345678901234567890123456789012 \\(now X123456789012345678901234567890\\)\\."
  )

  test_demo_df <- demo_df
  test_demo_df[1, "tab_title"] <- "Cover!"
  expect_warning(
    as_aftable(test_demo_df),
    "These tab_titles have been cleaned automatically: Cover! \\(now Cover\\)\\."
  )

  test_demo_df <- demo_df
  test_demo_df["tab_title"][5, ] <- long_title


  expect_warning(
    .warn_aftable(test_demo_df),
    "Each tab_title must be shorter than 31 characters."
  )

  test_demo_df <- demo_df
  test_demo_df["tab_title"][5, ] <- "Table-1!"

  expect_warning(
    .warn_aftable(test_demo_df),
    "Each tab_title must contain only letters, numbers or underscores."
  )
})

test_that("input column names are okay", {
  names(demo_df)[1] <- "x"
  expect_error(as_aftable(demo_df))
})

test_that("character class columns are caught if not character class", {
  demo_df[, "sheet_type"] <- seq_len(nrow(demo_df))
  expect_error(as_aftable(demo_df))
})
