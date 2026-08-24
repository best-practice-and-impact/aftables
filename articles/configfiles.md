# Config files

## Purpose

This vignette provides an overview of how to use a config file with the
{aftables} package to set workbook properties and formatting.

## Workbook properties and formatting which can be set in a config file

Workbook properties:

- Author
- Title
- Keywords
- Subject
- Category
- Comments

Analysis Function guidance advises workbooks should have the author,
title, keywords and language document properties completed. aftables
provides functionality to set the author, title and keywords properties.
See [‘Releasing statistics in
spreadsheets’](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/)
for more information including how to set the workbook language.

Workbook formatting:

- Font name
- Font sizes
- Column widths

## Structure of a config file

The config file must include an `aftables` entry. {aftables} will only
look for workbook configs below the `aftables` entry. The config file
may include any other entries. This means {aftables} settings can be
added to an existing config file without conflicts.

The `aftables` config file supports multiple entries. There must be at
least one config below the `aftables` entry, which could be named
`default` or be a user-specified custom config.

Each config must contain at least one of `workbook_properties` and
`workbook_format`. The value of each key below `workbook_properties`
must be a character string apart from `keywords`, which is a character
vector. The value of every key below `workbook_format` must be an
integer apart from `base_font_name` which must be a character string.
{aftables} checks all config keys to ensure they are character strings
or integers as required to process them correctly.

This is the contents of the example config file included with
{aftables}:

``` xml
aftables:
  default:
    workbook_properties:
      author: "aftables"
      title: "aftables example workbook"
      keywords:
        - "aftables1"
        - "aftables2"
        - "aftables3"
      subject: "aftables example subject"
      category: "aftables example category"
      comments: "aftables example comments"
    workbook_format:
      base_font_name: "Arial"
      base_font_size: 12
      table_header_size: 12
      sheet_heading_size: 16
      sheet_subheading_size: 14
      cellwidth_default: 16
      cellwidth_wider: 32
      nchar_break: 50
  workbook1:
    workbook_properties:
      category: "aftables workbook 1 category"
      keywords:
        - "aftablesworkbook1"
  workbook2:
    workbook_properties:
      title: "aftables workbook 2"
      category: "aftables workbook 2 category"
      keywords:
        - "aftablesworkbook2"
```

Values of keys below `workbook_properties` will appear in the Excel
workbook when it is saved using wb_save from openxlsx2. They can be
found in the file properties or the workbook information.

Values of keys below `workbook_format` will be applied to the contents
of the workbook. The values of `base_font_name` and `base_font_size`
define the default font name and size used by the workbook. All text not
formatted as a table header, sheet subheading or sheet heading will use
the default settings. Font sizes of sheet headings, sheet subheadings,
and table header rows will use the values of `sheet_heading_size`,
`sheet_subheading_size` and `table_header_size` respectively, and they
will additionally be formatted as bold.

The values of `cellwidth_default`, `cellwidth_wider` and `nchar_break`
are used to define column widths. The units of `cellwidth_default` and
`cellwidth_wider` are the column width values used by Excel. All columns
widths are set by default to use the `cellwidth_default` value. If the
number of characters in a column header or the contents of a column
exceeds the value of `nchar_break` aftables will set the column width to
the value of `cellwidth_wider`. Users can avoid text wrapping in columns
or column headers by setting the value of `nchar_break` based on their
data or the content of their column headers.

The
[`create_config_yaml()`](https://best-practice-and-impact.github.io/aftables/reference/create_config_yaml.md)
function outputs a copy of this config file to a location of the users
choice. The default location is the current working directory. We
recommend using the example config file as a basis for developing your
own {aftables} config file.

## Applying a config file with `generate_workbook()`

The
[`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
function has two arguments for config files: `config_path` and
`config_name`. Both arguments are optional, and {aftables} will take
different action based on which arguments are provided.

{aftables} will look for a config file using the `config_path` argument.
By default {aftables} will look in the current working directory for a
config file named `config.yaml`. `config_path` can be set to NULL and
{aftables} will not look for a config file.

{aftables} will only use the `config_name` argument if a config file is
found. {aftables} config files must have an `aftables` entry containing
one or more configs. These configs can be a `default` config and/or any
user-specified custom configs. Users can set up multiple custom configs
to produce multiple workbooks from a single config file. Settings common
to all workbooks would be set in the `default` config, and settings
specific to each workbook would be set in separate custom configs. The
`default` config is optional, and custom configs can be used to specify
all workbook properties and formatting. When a config file contains both
a `default` config and a user-specified custom config matching the
`custom_name` argument a combined config is generated from the `default`
config and the `config_name` custom config. Values specified in the
`config_name` custom config are preferred over the `default` config.

### Example 1: No config arguments in generate_workbook

``` r

my_wb <- generate_workbook(aftable = demo_aftable)
```

When the user does not provide any config arguments to {aftables} it
will look in the working directory for a `config.yaml` file. If
{aftables} finds a config file it will look for an `aftables` entry with
a `default` config. If {aftables} cannot find a config file or the
config file doesn’t contain an `aftables` entry with a `default` config,
it will generate an {openxlsx2} wbWorkbook object named `my_wb` without
applying any config. Otherwise {aftables} will generate an {openxlsx2}
wbWorkbook object named `my_wb` with the `default` config applied. This
is the default behaviour of the {generate_workbook} function.

### Example 2: Using a custom config

``` r

my_wb <- generate_workbook(
  aftable = demo_aftable,
  config_name = "workbook1"
)
```

{aftables} will look in the working directory for a file named
`config.yaml` and import the `aftables` entry. It will look for the
`workbook1` config as well as a `default` config. If there is no
`aftables` entry or `workbook1` config, {aftables} will stop with an
error. If both the `default` and `workbook1` configs exist {aftables}
will combine them, preferring values specified in the `config_name`
custom config over those in the `default` config. If there is no
`default` config all values will be taken from the `workbook1` config.
{aftables} will generate an {openxlsx2} wbWorkbook object named `my_wb`
with the `workbook1` or combined config applied.

### Example 3: Using a custom config file

``` r

dir.create("configs")

create_config_yaml(
  path = "configs",
  open_config = FALSE
)

my_wb <- generate_workbook(
  aftable = demo_aftable,
  config_path = "configs/config.yaml"
)
```

{aftables} will look for the user-specified config file from
`config_path` and import the `aftables` entry. It will look for the
`default` config. If there is no `aftables` entry or `default` config,
{aftables} will stop with an error. Otherwise {aftables} will generate
an {openxlsx2} wbWorkbook object named `my_wb` with the `default` config
applied.

### Example 4: Using a custom config file with a custom config

``` r

dir.create("configs")

create_config_yaml(
  path = "configs",
  open_config = FALSE
)

my_wb <- generate_workbook(
  aftable = demo_aftable,
  config_path = "configs/config.yaml",
  config_name = "workbook1"
)
```

{aftables} will look for the user-specified config file from
`config_path` and import the `aftables` entry. It will look for the
`workbook1` config as well as a `default` config. If there is no
`aftables` entry or `workbook1` config, {aftables} will stop with an
error. If both the `default` and `workbook1` configs exist {aftables}
will combine them, preferring values specified in the `config_name`
custom config over those in the `default` config. If there is no
`default` config all values will be taken from the `workbook1` config.
{aftables} will generate an {openxlsx2} wbWorkbook object named `my_wb`
with the `workbook1` or combined config applied.
