# Contributing to hubEnsembles

This outlines how to propose a change to `hubEnsembles`.
For more general info about contributing to this, and other hubverse packages, please see the
[**hubverse community page**](https://hubverse.io/community/).

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the *source* file.
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file.
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it's needed.
If you've found a bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

Our procedures for contributing bigger changes, code in particular, generally follow those advised by the tidyverse dev team, including following the tidyverse style guide for code and recording user facing changes in `NEWS.md`.

### Pull request process

- Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("hubverse-org/hubEnsembles", fork = TRUE)`.

- Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`.
  If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.

- Follow [the pull request checklist](https://hubverse-org.github.io/hubDevs/articles/release-checklists.html#subsequent-pr-checklist) to create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("name/brief-description/issue")`.

- Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
  The title of your PR should briefly describe the change.
  The body of your PR should contain `Fixes #issue-number`.

- For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first heading---usually labelled "development version"). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the tidyverse [style guide](https://style.tidyverse.org).
  You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.

- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.

- We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.
  Contributions with test cases included are easier to accept.

## Code of Conduct

Please note that the hubEnsembles project is released with a
[Contributor Code of Conduct](https://hubverse-org.github.io/hubEnsembles/CODE_OF_CONDUCT.html).
By contributing to this project you agree to abide by its terms.


