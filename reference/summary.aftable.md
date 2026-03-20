# Summarise An 'aftable' Object

A concise result summary of an aftable-class object to see information
about the sheet content. Shows a numbered list of sheets with each tab
title, sheet type and table dimensions.

## Usage

``` r
# S3 method for class 'aftable'
summary(object, ...)
```

## Arguments

- object:

  An aftable-class object for which to get a summary.

- ...:

  Other arguments to pass.

## Value

`object` unaltered.

## Examples

``` r
# Print a concise summary of the aftable-class object
summary(demo_aftable)
#> # An aftable with 5 sheets: 
#>   1) Tab 'Cover' (sheet type 'cover') contains a list of length 3 (element lengths 2, 1 and 2)
#>   2) Tab 'Contents' (sheet type 'contents') contains a 3 x 2 dataframe
#>   3) Tab 'Notes' (sheet type 'notes') contains a 3 x 2 dataframe
#>   4) Tab 'Table_1' (sheet type 'tables') contains a 10 x 7 dataframe
#>   5) Tab 'Table_2' (sheet type 'tables') contains a 10 x 2 dataframe

# Alternatively, look at the structure
str(demo_aftable, max.level = 2)
#> Classes ‘aftable’, ‘tbl’ and 'data.frame':   5 obs. of  7 variables:
#>  $ tab_title  : chr  "Cover" "Contents" "Notes" "Table_1" ...
#>  $ sheet_type : chr  "cover" "contents" "notes" "tables" ...
#>  $ sheet_title: chr  "The 'aftables' Demo Workbook" "Table of contents" "Notes" "Table_1: First Example Sheet" ...
#>  $ blank_cells: chr  NA NA NA "Blank cells indicate that there's no note in that row." ...
#>  $ source     : chr  NA NA NA "[The Source Material, 2024.](https://best-practice-and-impact.github.io/aftables/)" ...
#>  $ custom_rows:List of 5
#>   ..$ : chr NA
#>   ..$ : chr NA
#>   ..$ : chr "A custom row."
#>   ..$ : chr  "First custom row [with a hyperlink.](https://best-practice-and-impact.github.io/aftables/)" "Second custom row."
#>   ..$ : chr "A custom row."
#>  $ table      :List of 5
#>   ..$ :List of 3
#>   ..$ :'data.frame': 3 obs. of  2 variables:
#>   ..$ :'data.frame': 3 obs. of  2 variables:
#>   ..$ :'data.frame': 10 obs. of  7 variables:
#>   ..$ :'data.frame': 10 obs. of  2 variables:
```
