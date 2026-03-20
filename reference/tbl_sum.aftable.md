# Provide A Succinct Summary Of An 'aftable' Object

A brief text description of an aftable-class object.

## Usage

``` r
# S3 method for class 'aftable'
tbl_sum(x, ...)
```

## Arguments

- x:

  An aftable-class object to summarise.

- ...:

  Other arguments to pass.

## Value

Named character vector.

## Examples

``` r
# Print with description
print(demo_aftable)
#> # aftable: 5 x 7
#>   tab_title sheet_type sheet_title   blank_cells source custom_rows table       
#>   <chr>     <chr>      <chr>         <chr>       <chr>  <list>      <list>      
#> 1 Cover     cover      The 'aftable… NA          NA     <chr [1]>   <named list>
#> 2 Contents  contents   Table of con… NA          NA     <chr [1]>   <df [3 × 2]>
#> 3 Notes     notes      Notes         NA          NA     <chr [1]>   <df [3 × 2]>
#> 4 Table_1   tables     Table_1: Fir… Blank cell… [The … <chr [2]>   <df>        
#> 5 Table_2   tables     Table_2: Sec… NA          The S… <chr [1]>   <df>        

# Print description only (package 'tibble' must be installed)
tibble::tbl_sum(demo_aftable)
#> aftable 
#> "5 x 7" 
```
