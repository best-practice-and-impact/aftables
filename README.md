
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {a11ytables}

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/a11ytables)](https://CRAN.R-project.org/package=a11ytables)
[![R-CMD-check](https://github.com/co-analysis/a11ytables/workflows/R-CMD-check/badge.svg)](https://github.com/co-analysis/a11ytables/actions)
[![Codecov test
coverage](https://codecov.io/gh/co-analysis/a11ytables/branch/main/graph/badge.svg)](https://codecov.io/gh/co-analysis/a11ytables?branch=main)
<!-- badges: end -->

## Purpose

{a11ytables} is a work-in-progress R package built around
[{openxlsx}](https://ycphs.github.io/openxlsx/) that aims to comply with
the latest guidance (June 2021) on [releasing statistics in
spreadsheets](https://gss.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/)
from the [Best Practice and Impact
Division](https://github.com/best-practice-and-impact?language=html)
(BPID) of the UK’s [Government Statistical
Service](https://gss.civilservice.gov.uk/).

## Install and use

You can install {a11ytables} from GitHub using {remotes}. {openxlsx}
will be installed too.

``` r
install.packages("remotes")
remotes::install_github("co-analysis/a11ytables")
library(a11ytables)
```

To use:

1.  Create an a11ytables-class dataframe object containing your data
    (TODO: make the class)
2.  Pass that object to `create_a11y_wb()` to populate an {openxlsx}
    Workbook-class object
3.  Write that object with `openxlsx::saveWorkbook()`

## Detail

The information required to build the output can be arranged into a
single dataframe with class ‘a11ytables’ (TODO: explain how to construct
these objects). You pass that object to the `create_a11y_wb()` function,
which in turn creates an {openxlsx} Workbook-class object with the
required sheets, their content and styling in accordance with
accessibility best practice (TODO: make all features accessible).

Here is the built-in dataset `lfs_tables`, adapted from [the releasing
statistics in spreadsheets
guidance](https://gss.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/)
(TODO: actually convert this a11ytables-class):

``` r
dplyr::glimpse(tibble::as_tibble(lfs_tables))
#> Rows: 5
#> Columns: 8
#> $ tab_title      <chr> "cover", "contents", "notes", "1a", "2"
#> $ sheet_type     <chr> "meta", "meta", "meta", "tables", "tables"
#> $ sheet_title    <chr> "Labour market overview data tables, UK, December 2020 …
#> $ source         <chr> NA, NA, NA, "Labour Force Survey", "Labour Force Survey"
#> $ subtable_num   <chr> NA, NA, NA, NA, NA
#> $ subtable_title <chr> NA, NA, NA, NA, NA
#> $ table_name     <chr> "Cover_content", "Table_of_contents", "Notes_table", "L…
#> $ table          <list> [<tbl_df[17 x 1]>], [<tbl_df[2 x 5]>], [<tbl_df[11 x 2]…
```

<details>
<summary>
Click here to view an explanation of the variables.
</summary>

Columns in the a11ytables-class dataframe are:

-   `tab_title`: the name of the tab (i.e. the text that will appear on
    each tab of the output workbook), a column that must include ‘cover’
    and ‘contents’; optionally ‘notes’; and one row per table for
    publication (numbers are recommended for tables)
-   `sheet_type`: ‘cover’, ‘contents’ and ‘notes’ are ‘meta’ sheets;
    tables for publication are ‘tables’
-   `sheet_title`: the title of the sheet, displayed in cell A1
-   `source`: the provenance of the data (optional), displayed in cell
    A3
-   `table_name`: a name to give the marked-up table (no spaces), which
    will allow screenreaders to identify the table in the output
    workbook
-   `table`: a list-column containing a dataframe with the table to be
    added to a given sheet

Optionally, for tabs that contain more than one table (not recommended,
also TODO):

-   `subtable_num`: string to append to the tab\_title to identify this
    sub-table, for example ‘a’ and ‘b’
-   `subtable_title`: title to be added above the sub-table within the
    sheet

</details>
<p>

And so, this can be passed to a single function to create an {openxlsx}
Workbook-class object:

``` r
example_wb <- create_a11y_wb(lfs_tables)
```

<details>
<summary>
Click here to view the contents of the output.
</summary>

``` r
example_wb
#> A Workbook object.
#>  
#> Worksheets:
#>  Sheet 1: "cover"
#>  
#>  Custom row heights (row: height)
#>   3: 34, 5: 34, 7: 34, 9: 34, 11: 34, 13: 34, 15: 34, 17: 34 
#>  Custom column widths (column: width)
#>    1: 80 
#>  
#> 
#>  Sheet 2: "contents"
#>  
#>  Custom column widths (column: width)
#>    1: 30, 2: 30, 3: 30, 4: 30, 5: 30 
#>  
#> 
#>  Sheet 3: "notes"
#>  
#>  Custom column widths (column: width)
#>    1: 15, 2: 80 
#>  
#> 
#>  Sheet 4: "1a"
#>  
#>  Custom column widths (column: width)
#>    1: 16, 2: 16, 3: 16, 4: 16, 5: 16, 6: 16, 7: 16, 8: 16, 9: 16, 10: 16 
#>  
#> 
#>  Sheet 5: "2"
#>  
#>  Custom column widths (column: width)
#>    1: 16, 2: 16, 3: 16, 4: 16, 5: 16, 6: 16, 7: 16, 8: 16, 9: 16 
#>  
#> 
#>  
#>  Worksheet write order: 1, 2, 3, 4, 5
#>  Active Sheet 1: "cover" 
#>  Position: 1
```

</details>
<p>

This output can be written to disk or opened temporarily with
spreadsheet software.

``` r
openxlsx::openXL(example_wb)  # optionally open temp copy
openxlsx::saveWorkbook(example_wb, "publication.xlsx")
```

## Similar projects

BPID have themselves released [a Python package called
‘gptables’](https://github.com/best-practice-and-impact/gptables) to
help users produce such spreadsheets, but it’s based on a prior version
of the guidance. [That package will be
updated](https://github.com/best-practice-and-impact/gptables/issues/145),
but there’s a gap for this functionality—and for a dedicated R
package—at the time of writing (August 2021).

{a11ytables} is being developed with specific statistical releases in
mind, but the goal is to generalise it and perhaps have some
functionality absorbed into the development of
[gptables](https://github.com/best-practice-and-impact/gptables) in some
way.

The code is under development and current API features may change in
future releases. Please see the NEWS file for details of any breaking
changes.

## Code of Conduct

Please note that the {a11ytables} project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

Cabinet Office users, please adhere to [the co-analysis Git and GitHub
code of conduct and usage
policy](https://docs.google.com/document/d/1CuNgKla1BwSVOmGkPmsq0S-OM4emP-iXrgnm7EeILWM/edit?usp=sharing).

Please [file an
issue](https://github.com/co-analysis/csstatsbulletin/issues) where
possible and provide meaningful commentary in any commits and pull
requests.

## Copyright

Copyright (c) 2021 Crown Copyright (Cabinet Office).

The code in this repository is released under the MIT license as per
[The GDS
Way](https://gds-way.cloudapps.digital/manuals/licensing.html#use-mit).
