# Changelog

## aftables 2.0.0

- Major update of package back-end to use openxlsx2 functions to build
  and format workbook
  ([\#70](https://github.com/best-practice-and-impact/aftables/issues/70)).

- Bug fix: ensured columns containing both numbers/currencies and
  shorthand have numeric formatting applied to prevent Excel warning of
  numbers formatted as characters.
  ([\#93](https://github.com/best-practice-and-impact/aftables/issues/93))

- Bug fix: numeric formatting applies thousand separators by default.
  ([\#51](https://github.com/best-practice-and-impact/aftables/issues/51))

- Added new feature to set document properties and text formatting in a
  config.yaml file. The new function
  [`create_config_yaml()`](https://best-practice-and-impact.github.io/aftables/reference/create_config_yaml.md)
  exports an example `config.yaml` file for users to amend. New
  arguments `config_path` and `config_name` in
  [`generate_workbook()`](https://best-practice-and-impact.github.io/aftables/reference/generate_workbook.md)
  are available to users to specify configurations. Alternative
  arguments `author`, `title` and `keywords` are available to set
  minimum recommended workbook properties without using a `config.yaml`
  file.
  ([\#55](https://github.com/best-practice-and-impact/aftables/issues/55),
  [\#138](https://github.com/best-practice-and-impact/aftables/issues/138))

- Internal: Added code to generate aftables hex logo using {gex}
  ([\#134](https://github.com/best-practice-and-impact/aftables/issues/134))

## aftables 1.0.2

CRAN release: 2025-02-19

- Updated package name to aftables. Function names have been updated to
  remove references to a11ytables.
