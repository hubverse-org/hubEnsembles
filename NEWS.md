# hubEnsembles 0.1.8

* Update README to point to hubverse R-universe
* Submission to CRAN
* Remove unnecessary remotes for packages that have been added to CRAN

# hubEnsembles 0.1.7

* Make `hubEnsembles.Rmd` an article
* Rewrite `linear_pool_quantile()` to not use `Hmisc-utils` functions
* Fix bug in `linear_pool()` where the pools were not being properly split (#128)
* Fix warnings from dplyr about usage of `all_of()`

# hubEnsembles 0.1.6

* Fix bug in `simple_ensemble()` that produces invalid distributions for certain weighted medians (#122)
* Replace magrittr pipe (`%>%`) with base R 4.1 pipe (`|>`)
* Simplify examples

# hubEnsembles 0.1.5

* Check that weights don't depend on output type ID for PMF and CDF forecasts (#35)

# hubEnsembles 0.1.4

* Replace superseded `map_dfr()` call
* Replace usage of superseded ".data$"

# hubEnsembles 0.1.3

* Change organization name to "hubverse-org" (#115)

# hubEnsembles 0.1.2

* Updated vignette (#29, #113)
* Remove out-of-date example data (#113)
* Remove Hmisc dependency (#55)

# hubEnsembles 0.1.1

* Bumped hubUtils to 0.0.1 or higher, which has been split into hubUtils and hubData (#98)
* Bumped Roxygen to 7.3.1
* Add lint workflow (#96, #98)
* Upgrade GitHub workflows (#96, #98)

# hubEnsembles 0.1.0

* Add example data (#95)
* Upgrades package docs to hubStyle theme (#93)

# hubEnsembles 0.0.9001

* Fix handling of quantile output type in `linear_pool_quantile()` internal helper (#69)

# hubEnsembles 0.0.9000

* Initial Release.
