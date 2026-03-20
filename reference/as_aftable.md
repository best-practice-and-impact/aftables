# Coerce To An 'aftable' Object

Functions to check if an object is an aftable, or coerce it if possible.

## Usage

``` r
as_aftable(x)

is_aftable(x)
```

## Arguments

- x:

  A data.frame object to coerce.

## Value

`as_aftable` returns an object of class aftable if possible.
`is_aftable` returns `TRUE` if the object has class aftable, otherwise
`FALSE`.

## Examples

``` r
as_aftable(demo_df)
#> # aftable: 5 x 7
#>   tab_title sheet_type sheet_title   blank_cells source custom_rows table       
#>   <chr>     <chr>      <chr>         <chr>       <chr>  <list>      <list>      
#> 1 Cover     cover      The 'aftable… NA          NA     <chr [1]>   <named list>
#> 2 Contents  contents   Table of con… NA          NA     <chr [1]>   <df [3 × 2]>
#> 3 Notes     notes      Notes         NA          NA     <chr [1]>   <df [3 × 2]>
#> 4 Table_1   tables     Table_1: Fir… Blank cell… [The … <chr [2]>   <df>        
#> 5 Table_2   tables     Table_2: Sec… NA          The S… <chr [1]>   <df>        
is_aftable(demo_aftable)
#> [1] TRUE
```
