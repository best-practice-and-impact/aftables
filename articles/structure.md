# Package structure

## Purpose

This vignette is aimed at developers who want to understand the package
better and to make it easier for them to contribute.

## Overview

There are only two main user-facing functions in {aftables}:

- [`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
  to create a data.frame object (with an additional ‘aftable’ S3 class)
  filled with all the information needed to create a spreadsheet output,
  as well as check the validity of the structure and provide errors or
  warnings
- [`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
  to convert the output from
  [`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
  to an [{openxlsx2}](https://janmarvin.github.io/openxlsx2/)
  wbWorkbook-class object, ready for writing to an xlsx file with
  [`openxlsx2::wb_save()`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html)

This simplicity is a feature, not a bug. It’s designed to greatly
simplify the process of creating compliant spreadsheets. The package
does the hard work of making the outputs compliant so the user spends
less time dealing with it.

This vignette provides a quick look at what’s happening ‘under the hood’
in these functions.

Please add [an
issue](https://github.com/best-practice-and-impact/aftables/issues) to
the package’s GitHub repository if you would like any of this
explanation to be expanded, or provide a solution in [a pull
request](https://github.com/best-practice-and-impact/aftables/pulls).

## Files

First it’s worth explaining how the source files are laid out. There are
four major groups of scripts in the `R/` directory of the package:

1.  Code to make aftable-class objects: `aftable.R` and
    `utils-aftable.R` contain code for handling the aftable class, most
    importantly the
    [`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
    function, but also coercion with
    [`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md),
    checking with
    [`is_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md),
    a [`summary()`](https://rdrr.io/r/base/summary.html) method and a
    [`print()`](https://rdrr.io/r/base/print.html) method, which takes
    advantage of [the {pillar} package](https://github.com/r-lib/pillar)
    for prettier outputs.
2.  Code to make Workbook-class objects: `workbook.R`,
    `utils-workbook.R` and `utils-workbook-style.R` contain the code for
    creating and styling a Workbook-class object with the
    [`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
    function.
3.  Code to produce demo datasets: `data.R` contains the documentation
    for demo datasets, which are created in the `data-raw/` directory
    with the files stored in the `data/` directory.
4.  Code that creates the RStudio Addin: `addin.R` and `utils-addin.R`
    contain code for the [RStudio
    Addin](https://rstudio.github.io/rstudioaddins/) (the .dcf file for
    which is in the `inst/rstudio/` directory).

You’ll also find the `aftables-package.R` file in the `R/` directory,
which provides a package-level help page derived from the DESCRIPTION
file when
[`?aftables`](https://best-practice-and-impact.github.io/aftables/reference/aftables-package.md)
is run by the user. It doesn’t need to be edited.

## Code

This sections below focus on the
[`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
and
[`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
functions, which are the primary and most complex functions in the
package.

The code that underpins these functions is modularised to aid with
bug-catching and testing, but also to make it easier for developers to
understand how the code fits together. Internal sub-functions are
consistently-named and begin with verbs, which should help you better
understand their purpose.

Note that {aftables} uses a convention that internal functions
(i.e. those not presented to the user, but accessed via the `:::`
qualifier) are prefixed with a period (i.e. `.f()`) to make it clearer
that they are internal to the package. The exported user-facing
functions do not use a leading period.

### To create aftables

Actually,
[`create_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
itself only does one thing: it takes user inputs from the arguments and
combines them into a dataframe. It then passes this off to the most
important function in the package,
[`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md),
which is responsible for coercing the dataframe to aftable class and
performing checks on its content.

Basically,
[`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)
creates an S3-class object with classes ‘data.frame’ and ‘tbl’
(i.e. [tibble](https://tibble.tidyverse.org/)) and an additional
‘aftable’ class.

``` r
library(aftables)
my_aftable <- as_aftable(demo_df)
class(my_aftable)
# [1] "aftable"    "tbl"        "data.frame"
```

The object can be manipulated like a ‘normal’ dataframe and—thanks to
the {pillar} package and the tbl class—it can be printed in compact form
without the need for the whole of the {tibble} package to be imported.

``` r
my_aftable
# # aftable: 5 x 7
#   tab_title sheet_type sheet_title   blank_cells source custom_rows table       
#   <chr>     <chr>      <chr>         <chr>       <chr>  <list>      <list>      
# 1 Cover     cover      The 'aftable… NA          NA     <chr [1]>   <named list>
# 2 Contents  contents   Table of con… NA          NA     <chr [1]>   <df [3 × 2]>
# 3 Notes     notes      Notes         NA          NA     <chr [1]>   <df [3 × 2]>
# 4 Table_1   tables     Table_1: Fir… Blank cell… [The … <chr [2]>   <df>        
# 5 Table_2   tables     Table_2: Sec… NA          The S… <chr [1]>   <df>
```

Compare this to its appearance as a regular data.frame, which is
trickier to understand:

``` r
as.data.frame(my_aftable)
#   tab_title sheet_type                   sheet_title
# 1     Cover      cover  The 'aftables' Demo Workbook
# 2  Contents   contents             Table of contents
# 3     Notes      notes                         Notes
# 4   Table_1     tables  Table_1: First Example Sheet
# 5   Table_2     tables Table_2: Second Example Sheet
#                                              blank_cells
# 1                                                   <NA>
# 2                                                   <NA>
# 3                                                   <NA>
# 4 Blank cells indicate that there's no note in that row.
# 5                                                   <NA>
#                                                                               source
# 1                                                                               <NA>
# 2                                                                               <NA>
# 3                                                                               <NA>
# 4 [The Source Material, 2024.](https://best-practice-and-impact.github.io/aftables/)
# 5                                                         The Source Material, 2024.
#                                                                                                      custom_rows
# 1                                                                                                             NA
# 2                                                                                                             NA
# 3                                                                                                  A custom row.
# 4 First custom row [with a hyperlink.](https://best-practice-and-impact.github.io/aftables/), Second custom row.
# 5                                                                                                  A custom row.
#                                                                                                                                                                                                                                                                                                                                              table
# 1                                                                                                                                                First row of Section 1., Second row of Section 1., The only row of Section 2., [Website](https://best-practice-and-impact.github.io/aftables/), [Email address](mailto:fake.address@aftables.com)
# 2                                                                                                                                                                                                                                                  Notes, Table 1, Table 2, Notes used in this workbook, First Example Sheet, Second Example Sheet
# 3                                                                                                                                                                                                                                                                             [note 1], [note 2], [note 3], First note., Second note., Third note.
# 4 A, B, C, D, E, F, G, H, I, J, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, [c], 6, 7, 8, 9, [x], 59180, 29260, 92130, 24000, 91970, 78650, 281050, 97720, 174630, 15230, 0.56094, 0.27176, 0.47238, 0.62704, 0.01278, 0.01788, 0.31304, 1.276, 0.54384, 0.81019, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, [note 2], NA, NA, NA, NA, [note 3], NA, NA, NA, NA
# 5                                                                                                                                                                                                                                                                                      A, B, C, D, E, F, G, H, I, J, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```

Within
[`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)
itself are two major functions that help ensure proper construction of
an aftable object:

- `.validate_aftable()` will generate errors if basic structural
  expectations of an aftable aren’t met (e.g. if ‘cover’, ‘contents’ or
  ‘notes’ have been provided more than once to the `sheet_type`
  argument)
- `.warn_aftable()` checks for things that the user may have forgotten
  and prints warnings about them (e.g. if 5 notes are declared in the
  notes sheet but there are fewer in the tables themselves)

Advanced users can create a correctly-formatted data.frame on the fly
and convert it to an aftable with
[`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)
directly. The
[`as_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)
function mainly exists to make testing easier, i.e. you can pass to it
the pre-prepared `demo_df` dataset.

#### Methods

There’s a few methods for aftables that are also found in
`R/aftables.R`.

[`is_aftable()`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)
is a classic logical test that checks for the aftable class in the
object provided to it.

``` r
is_aftable(my_aftable)
# [1] TRUE
```

The [`summary()`](https://rdrr.io/r/base/summary.html) method prints a
very simple overview of a provided aftable.

``` r
summary(my_aftable)
# # An aftable with 5 sheets: 
#   1) Tab 'Cover' (sheet type 'cover') contains a list of length 3 (element lengths 2, 1 and 2)
#   2) Tab 'Contents' (sheet type 'contents') contains a 3 x 2 dataframe
#   3) Tab 'Notes' (sheet type 'notes') contains a 3 x 2 dataframe
#   4) Tab 'Table_1' (sheet type 'tables') contains a 10 x 7 dataframe
#   5) Tab 'Table_2' (sheet type 'tables') contains a 10 x 2 dataframe
```

The `tbl_sum()` method is provided via the {pillar} package, with the
goal of providing a bespoke header to the printed aftable.

``` r
pillar::tbl_sum(my_aftable)
# aftable 
# "5 x 7"
```

### To create workbooks

The
[`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
function sets up an
[{openxlsx2}](https://janmarvin.github.io/openxlsx2/) wbWorkbook-class
object and fills it by iterating over a user-supplied the aftable-class
object.

``` r
my_wb <- generate_workbook(my_aftable)
# Warning: Some of the recommended workbook properties are missing. Analysis
# Function guidance recommends completing the author, title and keywords fields.
class(my_wb)
# [1] "wbWorkbook" "R6"
```

You can see how the Workbook-class object carries information that will
determine the structure and style of the final spreadsheet output.

``` r
my_wb
# A Workbook object.
#  
# Worksheets:
#  Sheets: Cover, Contents, Notes, Table_1, Table_2 
#  Write order: 1, 2, 3, 4, 5
```

Several internal sub-functions within
[`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)—`.add_*()`,
`.insert_*()` and `.style_*()`—are responsible for adding these sheets,
inserting sheet elements and styling them, respectively.

#### Add sheets

A Workbook-class object is first created with
[`openxlsx2::wb_workbook()`](https://janmarvin.github.io/openxlsx2/reference/wb_workbook.html)
and then sheets are added based on the contents of the user-supplied
aftable.

The following functions add sheets and sheet elements into the workbook:

- `.add_tabs()` adds the required number of tabs into the workbook with
  [`openxlsx2::wb_add_worksheet()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_worksheet.html)
  (as per the `tab_title` column of the supplied aftable)
- `.add_cover()` and `.add_contents()` add the information needed for
  the cover and contents sheets (as per the required ‘cover’ and
  ‘contents’ supplied in the `sheet_type` column of an aftable)
- `.add_notes()` if a notes sheet exists (i.e. a row in the supplied
  aftable with a `sheet_type` of ‘notes’)
- `.add_table()` adds sheets for each statistical table (as per rows of
  supplied aftable with a `sheet_type` of ‘table’)

As sheets are added, content is inserted and styles are applied with
the:

- `.insert_*()` functions, which insert sheet elements (title, source
  statement, table, etc) to each sheet
- `.style_*()` functions, which apply formatting to each sheet
  (e.g. bold sheet titles with larger font) and the workbook (e.g. Arial
  font)

##### Insert sheet elements

There are several `.insert_*()` functions that add information to each
sheet depending on the `sheet_type` of the provided aftable, as well as
the content, if any, of its `sheet_title`, `blank_cells`, `source` and
`table` columns.

The following functions insert ‘pre-table’ elements in this order:

- `.insert_title()` to place the sheet title in cell A1
- `.insert_table_count()` to add a statement about the number of tables
  in the sheet
- `.insert_notes_statement()` if a `sheet_type` of ‘notes’ is provided
  in the user’s aftable
- `.insert_blanks_message()` if content is provided in the
  `blanks_cells` column of the user’s aftable
- `.insert_custom_rows()` if content is provided in the `custom_rows`
  column of the user’s aftable
- `.insert_source()` if content is provided in the `source` column of
  the user’s aftable

A table of data is added under the metadata with `.insert_table()`,
which is provided in the `table` column of the user’s aftable object.

The exact `.insert_*()` functions called depend on the `sheet_type`
declared in the aftable:

- meta sheets (cover, contents and notes) need only `.insert_title()`
  and `.insert_table_count()`
- statistical tables may also require `.insert_blanks_message()`,
  `.insert_custom_rows()` and `.insert_source()` if the relevant content
  is provided by the user, as well as `.insert_notes_statement()` if
  there are notes

Simple logic is used to check for the presence of meta elements with the
`.has_*()` functions, while the `get_start_row_*()` functions handle the
cell to which each message should be inserted.

For example, if all the elements are supplied, then the table would
begin in row 6 (i.e. after the sheet title, table count, note presence,
meaning of blank cells and source), but it’s possible that the table
would have to be inserted to row 3 if only the sheet title and statement
are required. This avoids inaccessible blank rows and redundant
statements like ‘This table has no source statement’.

##### Apply styles

There are a few `.style_*()` functions that create styles and apply them
on the basis of the `sheet_type` provided in the aftable.

- `.style_paragraph()` creates an easily-referenced lookup of paragraph
  styles.
- `.style_font()` creates an easily-referenced lookup of font styles
- `.style_workbook()` applies defaults for *the whole workbook* (i.e. to
  set the font style to Arial size 12)
- `.style_cover()`, `.style_contents()` and `.style_notes()` all apply
  styles to specific *sheets*
- `.style_sheet_title()` and `.style_table()` apply styles to
  *particular sheet elements* (e.g. the title is larger and bolder than
  the default font)

## Contribute

To contribute, please add [an
issue](https://github.com/best-practice-and-impact/aftables/issues) or
[a pull
request](https://github.com/best-practice-and-impact/aftables/pulls)
after reading [the code of
conduct](https://github.com/best-practice-and-impact/aftables/blob/main/.github/CODE_OF_CONDUCT.md)
and
[contributing](https://github.com/best-practice-and-impact/aftables/blob/main/.github/CONTRIBUTING.md)
guidance.
