
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {aftables} <a href="https://best-practice-and-impact.github.io/aftables/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/aftables)](https://CRAN.R-project.org/package=aftables)
[![R-CMD-check](https://github.com/best-practice-and-impact/aftables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/best-practice-and-impact/aftables/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Purpose

An R package to help automatically create reproducible spreadsheets that
adhere to the guidance on [releasing statistics in
spreadsheets](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/)
from the UK government’s [Analysis
Function](https://analysisfunction.civilservice.gov.uk/), with a focus
on accessibility.

Visit [the {aftables}
website](https://best-practice-and-impact.github.io/aftables/) for
documentation.

## Accessibility

This package is not yet capable of creating perfectly accessible
spreadsheets but will help with the bulk of the work needed. Users of
this packages should refer back to [the main spreadsheet
guidance](https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/)
or [the spreadsheet accessibility
checklist](https://analysisfunction.civilservice.gov.uk/policy-store/making-spreadsheets-accessible-a-brief-checklist-of-the-basics/)
after using it to make sure nothing has been missed. Please email
<analysis.function@ons.gov.uk> if you use the package so they can
monitor use and the outputs produced.

## Contribute

The package is under (opinionated) active development. Please see [the
NEWS
file](https://best-practice-and-impact.github.io/aftables/news/index.html)
for the latest changes.

To contribute, please add [an
issue](https://github.com/best-practice-and-impact/aftables/issues) or
[a pull
request](https://github.com/best-practice-and-impact/aftables/pulls)
after reading [the code of
conduct](https://github.com/best-practice-and-impact/aftables/blob/main/CODE_OF_CONDUCT.md)
and
[contributing](https://github.com/best-practice-and-impact/aftables/blob/main/.github/CONTRIBUTING.md)
guidance.

## Install

Install the package [from
GitHub](https://github.com/best-practice-and-impact/aftables) using
[{remotes}](https://remotes.r-lib.org/).

``` r
install.packages("remotes")  # if not already installed

remotes::install_github(
  repo = "best-practice-and-impact/aftables",  # GitHub user/repository
  dependencies = TRUE,              # install required/suggested packages
  build_vignettes = TRUE            # generate vignette documentation
)

library(aftables)  # attach package
```

## How to install a11ytables

The original name of the aftables package was a11ytables. The package
was renamed to make it consistent the [afcharts
package](https://github.com/best-practice-and-impact/afcharts). Some
function names were updated when the package was renamed. If you need to
install a11ytables for a legacy project, the following code can be used.

``` r
install.packages("remotes")  # if not already installed

remotes::install_github(
  repo = "best-practice-and-impact/aftables",  # GitHub user/repository
  ref = "v0.3.2", # this installs the final version of a11ytables. Change if you need an earlier version.
  dependencies = TRUE,              # install required/suggested packages
  build_vignettes = TRUE            # generate vignette documentation
)

library(a11ytables)  # attach package
```

## Use

To create a spreadsheet:

1.  Use `create_aftable()`
2.  Pass the output to `generate_workbook()`
3.  Pass the output to `openxlsx::saveWorkbook()`

Run `?function_name` or visit [the package
website](https://best-practice-and-impact.github.io/aftables/reference/index.html)
for function documentation. For long-form documentation, [visit the
package website](https://best-practice-and-impact.github.io/aftables/)
or run `browseVignettes("aftables")` to read the:

- [introductory
  vignette](https://best-practice-and-impact.github.io/aftables/articles/aftables.html)
  to get started
- [accessibility checklist
  vignette](https://best-practice-and-impact.github.io/aftables/articles/checklist.html)
  to see how the package complies with best-practice guidance
- [terminology
  vignette](https://best-practice-and-impact.github.io/aftables/articles/terminology)
  to understand the nomenclature of spreadsheet terms as used in this
  package
- [package structure
  vignette](https://best-practice-and-impact.github.io/aftables/articles/structure)
  to see how the package works under the hood

This package also includes [an RStudio
Addin](https://rstudio.github.io/rstudioaddins/) that inserts pre-filled
demo skeletons of the {aftables} workflow.

## Related projects

The ONS’s Analysis Standards and Pipelines team has released [a Python
package called
‘gptables’](https://github.com/best-practice-and-impact/gptables).
{aftables} is an independent effort that offers a native R solution that
is very similar to gptables in its outputs, though there are some
differences in implementation.

{aftables} can help you fulfill a [Reproducible Analytical
Pipeline](https://analysisfunction.civilservice.gov.uk/support/reproducible-analytical-pipelines/)
by automating the generation of compliant spreadsheets for publication.

## Code of Conduct

Please note that the {aftables} project is released with a [Contributor
Code of
Conduct](https://best-practice-and-impact.github.io/aftables/CODE_OF_CONDUCT.html).

## Copyright and Licensing

© Crown Copyright, 2023.

This work is [Crown
Copyright](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/).
The source code for the software is released under the MIT licence as
per the the [UK Government Licensing
Framework](https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/open-government-licence/open-software-licences/)
and the [GDS Way licensing
guidance](https://gds-way.digital.cabinet-office.gov.uk/manuals/licensing.html).
The documentation for the software is released under the [Open
Government
Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
