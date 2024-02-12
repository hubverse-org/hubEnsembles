
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hubEnsembles <img src="man/figures/logo.png" align="right" height="131" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/hubEnsembles)](https://CRAN.R-project.org/package=hubEnsembles)
[![R-CMD-check](https://github.com/Infectious-Disease-Modeling-Hubs/hubEnsembles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Infectious-Disease-Modeling-Hubs/hubEnsembles/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of hubEnsembles is to provide standard implementations of
commonly used methods for ensembling model outputs. The hubEnsembles
package is part of [the hubverse
project](https://hubdocs.readthedocs.io/en/latest/) and expects all
input data to the key functions to be formatted as an object of a
[`model_out_tbl`
class](https://infectious-disease-modeling-hubs.github.io/hubUtils/reference/as_model_out_tbl.html).

## Installation

You can install the development version of hubEnsembles using this code:

``` r
remotes::install_github("infectious-disease-modeling-hubs/hubEnsembles")
```

<!--
## Example
&#10;This is a basic example which shows you how to solve a common problem:
&#10;
```r
library(hubEnsembles)
## basic example code
```
&#10;What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:
&#10;
```r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```
&#10;You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.
&#10;You can also embed plots, for example:
&#10;<img src="man/figures/README-pressure-1.png" width="100%" />
&#10;In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

------------------------------------------------------------------------

## Code of Conduct

Please note that the hubEnsembles package is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.

## Contributing

Interested in contributing back to the open-source Hubverse project?
Learn more about how to [get involved in the Hubverse
Community](https://hubdocs.readthedocs.io/en/latest/overview/contribute.html)
or [how to contribute to the hubEnsembles
package](.github/CONTRIBUTING.md).
