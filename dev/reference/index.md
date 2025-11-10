# Package index

## All functions

- [`component_outputs`](https://hubverse-org.github.io/hubEnsembles/dev/reference/component_outputs.md)
  :

  Example model output data for
  [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)

- [`fweights`](https://hubverse-org.github.io/hubEnsembles/dev/reference/fweights.md)
  :

  Example weights data for
  [`simple_ensemble()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/simple_ensemble.md)

- [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
  :

  Compute ensemble model outputs as a linear pool, otherwise known as a
  distributional mixture, of component model outputs for each
  combination of model task, output type, and output type id. Supported
  output types include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.

- [`make_sample_indices_unique()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/make_sample_indices_unique.md)
  : Make the output type ID values of sample forecasts distinct for
  different models

- [`model_outputs`](https://hubverse-org.github.io/hubEnsembles/dev/reference/model_outputs.md)
  :

  Example model output data for
  [`simple_ensemble()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/simple_ensemble.md)

- [`simple_ensemble()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/simple_ensemble.md)
  :

  Compute ensemble model outputs by summarizing component model outputs
  for each combination of model task, output type, and output type id.
  Supported output types include `mean`, `median`, `quantile`, `cdf`,
  and `pmf`.

- [`validate_compound_taskid_set()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/validate_compound_taskid_set.md)
  :

  Perform validations on the compound task ID set used to calculate an
  ensemble of component model outputs for the sample output type,
  including checks that (1) `compound_taskid_set` is a subset of
  `task_id_cols`, (2) the provided `model_out_tbl` is compatible with
  the specified `compound_taskid_set`, and (3) all models submit
  predictions for the same set of non `compound_taskid_set` variables.

- [`weights`](https://hubverse-org.github.io/hubEnsembles/dev/reference/weights.md)
  :

  Example weights data for
  [`linear_pool()`](https://hubverse-org.github.io/hubEnsembles/dev/reference/linear_pool.md)
