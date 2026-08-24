# Helper function to specify how `aftables` should display numeric data

Control how `aftables` formats numeric data in tables in the excel
output. Set the number of decimal places to display and whether to use
commas as thousand separators. This function can be used to overwrite
the default behaviour of `aftables`, which normally determines the
number of decimal places required automatically from the data in each
numeric column, and adds thousand separators to all numeric columns. A
key use case for this function is to prevent `aftables` from displaying
calendar or financial years as numbers with thousand separators, but it
can be used with any numeric data column. Use this function before the
data frame is passed to the
[`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
function.

## Usage

``` r
number_formatter(table, columns, decimal_places = NA, thousand_separators = NA)
```

## Arguments

- table:

  Required data frame. Data frame to be passed into
  aftables::create_aftable function. No default.

- columns:

  Required character vector of column names or
  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/select.html)
  syntax determining columns to be processed with specified number
  formatting. No default.

- decimal_places:

  Optional numeric value specifying decimal places to display. Default
  `NA` keeps any existing custom number formatting already applied to
  the column. Set to `NULL` to remove any custom number formatting and
  for aftables to set the number of decimal places automatically.

- thousand_separators:

  Optional logical value whether data in specified columns should be
  formatted with thousand separators. Default `NA` keeps any existing
  custom number formatting already applied to the column. Set to `NULL`
  to remove any custom number formatting and for aftables to set
  thousand separators automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

set.seed(1066)

table_1_df <- data.frame(
  Category = LETTERS[1:10],
  Date = 2001:2010,
  Date2 = 2001:2010,
  "Count" = abs(round(rnorm(10), 3) * 1e3),
  "Population" = abs(round(rnorm(10), 5) * 1e5),
  check.names = FALSE
) |>
  mutate(Percentage = Count/Population * 100)

# Specify removing thousand separators for Date and Date2 columns
table_1_df_formatted <- table_1_df |>
  number_formatter(
    columns = c(Date, Date2),
    thousand_separators = FALSE
  )

# number_formatter columns argument also accepts tidyselect expressions
table_1_df_formatted_tidyselect <- table_1_df |>
number_formatter(
  columns = tidyselect::starts_with("Date"),
  thousand_separators = FALSE
)

# Display only 2 decimal places of Percentage column
table_1_df_formatted_2dp <- table_1_df |>
  number_formatter(
    columns = "Percentage",
    decimal_places = 2
  )

# number_formatter can be used multiple times to set different number formats
table_1_df_multiple_formats <- table_1_df |>
  number_formatter(
    columns = "Percentage",
    decimal_places = 2
  ) |>
  number_formatter(
    columns = tidyselect::starts_with("Date"),
    thousand_separators = FALSE
  )

} # }
```
