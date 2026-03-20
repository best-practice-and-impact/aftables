# A Demo 'aftables' Object

A pre-created 'aftables' object ready to be converted to an 'openxlsx2'
wbWorkbook-class object with
[`generate_workbook`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md).

## Usage

``` r
demo_aftable
```

## Format

A data.frame with 6 rows and 7 columns:

- tab_title:

  Character. Text to appear on each sheet's tab.

- sheet_type:

  Character. The content type for each sheet: 'cover', 'contents',
  'notes', or 'tables'.

- sheet_title:

  Character. The title that will appear in cell A1 (top-left) of each
  sheet.

- blank_cells:

  Character. An explanation for any blank cells in the table.

- custom_rows:

  List-column of character vectors. Additional arbitrary pre-table
  information provided by the user.

- source:

  Character. The origin of the data, if relevant.

- table:

  List-column of data.frames (apart from the cover, which is a list)
  containing the statistical tables.
