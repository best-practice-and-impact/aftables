# Generate A Workbook Object From An 'aftable'

Populate an 'openxlsx2' wbWorkbook-class object with content from an
aftable-class object. In turn, the output can be passed to
[`wb_save`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html)
from 'openxlsx2'

## Usage

``` r
generate_workbook(
  aftable,
  author = NULL,
  title = NULL,
  keywords = NULL,
  config_path = "config.yaml",
  config_name = NULL
)
```

## Arguments

- aftable:

  An aftable-class object created using
  [`create_aftable`](https://best-practice-and-impact.github.io/aftables/reference/create_aftable.md)
  (or
  [`as_aftable`](https://best-practice-and-impact.github.io/aftables/reference/as_aftable.md)),
  which contains the data and information needed to create a workbook.

- author:

  Optional character string to set the workbook author. Default NULL.

- title:

  Optional character string to set the workbook title. Default NULL.

- keywords:

  Optional character vector to set the workbook keywords. Default NULL.

- config_path:

  Optional character string containing path to config file. Defaults to
  config.yaml file located in working directory.

- config_name:

  Optional character string specifying which configuration to use from
  config file. Default NULL.

## Value

An openxlsx2 wbWorkbook-class object.

## Details

Analysis Function guidance advises workbooks should have the author,
title, keywords and language document properties completed. `aftables`
provides functionality to set the author, title and keywords properties.
See [Releasing statistics in
spreadsheets](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/#section-15)
for more information including how to set the workbook language.

A config file can be used to set workbook properties and formatting. See
[`create_config_yaml`](https://best-practice-and-impact.github.io/aftables/reference/create_config_yaml.md)
for details of how to create a config.yaml file. If author, title or
keywords are provided in both the `config.yaml` file and in the
`generate_workbook` arguments, the values provided in the function
arguments are preferred to those provided in the `config.yaml` file.

## Examples

``` r
# Convert an aftable to an openxlsx2 wbWorkbook-class object
# Setting the minimum workbook properties as function arguments
if (FALSE) { # \dontrun{
example_workbook <- generate_workbook(
  demo_aftable,
  author = "Example author",
  title = "example workbook",
  keywords = c("keyword1", "keyword2", "keyword3")
)} # }

# Use openxlsx2::wb_get_properties to view properties that have been applied
if (FALSE) { # \dontrun{
openxlsx2::wb_get_properties(example_workbook)} # }

# Save the workbook with openxlsx2::wb_save
if (FALSE) { # \dontrun{
openxlsx2::wb_save(example_workbook, "example_workbook.xlsx")} # }

# Using config.yaml file to set workbook properties and edit text and cell
# formatting
if (FALSE) { # \dontrun{
example_workbook2 <- generate_workbook(
  demo_aftable,
  config_path = system.file("ext-data", "config.yaml", package = "aftables"),
  config_name = "workbook1"
  )} # }

# Use openxlsx2::wb_get_properties to view properties that have been applied
if (FALSE) { # \dontrun{
openxlsx2::wb_get_properties(example_workbook2)} # }
```
