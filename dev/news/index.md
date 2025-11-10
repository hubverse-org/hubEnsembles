# Changelog

## hubEnsembles (development version)

## hubEnsembles 1.0.0

CRAN release: 2025-05-23

- `hubEnsembles.Rmd` article now explains how to ensemble samples using
  [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  supports requesting a subset of component model sample forecasts when
  ensembling samples
  ([\#144](https://github.com/hubverse-org/hubEnsembles/issues/144))
- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  supports the specification of the compound task ID set, so that
  trajectory samples can be correctly ensembled
  ([\#144](https://github.com/hubverse-org/hubEnsembles/issues/144))
- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  supports the simplest case of ensembling samples, where all component
  samples are collected and returned
  ([\#109](https://github.com/hubverse-org/hubEnsembles/issues/109))
- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  now uses the argument `derived_task_ids` (`derived_tasks` is now
  deprecated)
  ([\#156](https://github.com/hubverse-org/hubEnsembles/issues/156))

## hubEnsembles 0.1.9

CRAN release: 2024-10-02

- [`simple_ensemble()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/simple_ensemble.md)
  now uses [`identical()`](https://rdrr.io/r/base/identical.html) to
  avoid triggering an
  [`all.equal.environment()`](https://rdrr.io/r/base/all.equal.html)
  error. This error would sometimes occur when providing the `agg_fun`
  argument with a custom function.
  ([\#134](https://github.com/hubverse-org/hubEnsembles/issues/134))

## hubEnsembles 0.1.8

CRAN release: 2024-09-26

- README now points to hubverse R-universe
- Package submission to CRAN

## hubEnsembles 0.1.7

- `hubEnsembles.Rmd` vignette is now an article
- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  now properly splits its pools
  ([\#128](https://github.com/hubverse-org/hubEnsembles/issues/128))
- `linear_pool_quantile()` uses internal package functions only, not
  `Hmisc-utils` functions
- Functions using
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) are
  updated to avoid throwing dplyr warnings

## hubEnsembles 0.1.6

- Base R 4.1 pipe (`|>`) is used in place of magrittr pipe (`%>%`)
- Function examples are simplified
- [`simple_ensemble()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/simple_ensemble.md)
  now produces valid distributions for all weighted medians
  ([\#122](https://github.com/hubverse-org/hubEnsembles/issues/122))

## hubEnsembles 0.1.5

- Validate that `weights` argument doesn’t contain weights dependent on
  output type ID for PMF and CDF forecasts
  ([\#35](https://github.com/hubverse-org/hubEnsembles/issues/35))

## hubEnsembles 0.1.4

- Functions now use `map()` and `list_rbind()` in conjunction to avoid
  superseded warnings from purrr
  ([\#117](https://github.com/hubverse-org/hubEnsembles/issues/117))
- Functions now use double quotes or `.data[[]]` as appropriate within
  dplyr functions to avoid warnings
  ([\#117](https://github.com/hubverse-org/hubEnsembles/issues/117))

## hubEnsembles 0.1.3

- Organization name has been changed to “hubverse-org”
  ([\#115](https://github.com/hubverse-org/hubEnsembles/issues/115))

## hubEnsembles 0.1.2

- `hubEnsembles.Rmd` vignette now better reflects package capabilities
  ([\#29](https://github.com/hubverse-org/hubEnsembles/issues/29),
  [\#113](https://github.com/hubverse-org/hubEnsembles/issues/113))
- Example data that is out of date has been removed
  ([\#113](https://github.com/hubverse-org/hubEnsembles/issues/113))
- Hmisc dependency has been removed
  ([\#55](https://github.com/hubverse-org/hubEnsembles/issues/55))

## hubEnsembles 0.1.1

- hubUtils dependency has been bumped to 0.0.1 or higher, after its
  split into hubUtils and hubData
  ([\#98](https://github.com/hubverse-org/hubEnsembles/issues/98))
- Roxygen is bumped to 7.3.1
- Lint workflow have been added
  ([\#96](https://github.com/hubverse-org/hubEnsembles/issues/96),
  [\#98](https://github.com/hubverse-org/hubEnsembles/issues/98))
- GitHub workflows have been upgraded
  ([\#96](https://github.com/hubverse-org/hubEnsembles/issues/96),
  [\#98](https://github.com/hubverse-org/hubEnsembles/issues/98))

## hubEnsembles 0.1.0

- Example data has been added
  ([\#95](https://github.com/hubverse-org/hubEnsembles/issues/95))
- Package docs are upgraded to hubStyle theme
  ([\#93](https://github.com/hubverse-org/hubEnsembles/issues/93))

## hubEnsembles 0.0.9001

- `linear_pool_quantile()` now coerces quantile levels to numeric to
  prevent distfromq errors
  ([\#58](https://github.com/hubverse-org/hubEnsembles/issues/58),
  [\#63](https://github.com/hubverse-org/hubEnsembles/issues/63))

## hubEnsembles 0.0.9000

- Initial Release.
