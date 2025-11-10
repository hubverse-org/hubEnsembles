# Perform validations on the compound task ID set used to calculate an ensemble of component model outputs for the sample output type, including checks that (1) `compound_taskid_set` is a subset of `task_id_cols`, (2) the provided `model_out_tbl` is compatible with the specified `compound_taskid_set`, and (3) all models submit predictions for the same set of non `compound_taskid_set` variables.

Perform validations on the compound task ID set used to calculate an
ensemble of component model outputs for the sample output type,
including checks that (1) `compound_taskid_set` is a subset of
`task_id_cols`, (2) the provided `model_out_tbl` is compatible with the
specified `compound_taskid_set`, and (3) all models submit predictions
for the same set of non `compound_taskid_set` variables.

## Usage

``` r
validate_compound_taskid_set(
  model_out_tbl,
  task_id_cols,
  compound_taskid_set,
  derived_task_ids = NULL,
  return_missing_combos = FALSE
)
```

## Arguments

- model_out_tbl:

  an object of class `model_out_tbl` with component model outputs (e.g.,
  predictions).

- task_id_cols:

  `character` vector with names of columns in `model_out_tbl` that
  specify modeling tasks. Defaults to `NULL`, in which case all columns
  in `model_out_tbl` other than `"model_id"`, `"output_type"`,
  `"output_type_id"`, and `"value"` are used as task ids.

- compound_taskid_set:

  `character` vector of the compound task ID variable set. This argument
  is only relevant for `output_type` `"sample"`. Can be one of three
  possible values, with the following meanings:

  - `NA`: the compound_taskid_set is not relevant for the current
    modeling task

  - `NULL`: samples are from a multivariate joint distribution across
    all levels of all task id variables

  - Equality to `task_id_cols`: samples are from separate univariate
    distributions for each individual prediction task

  Defaults to NA. Derived task ids must be included if all of the task
  ids their values depend on are part of the `compound_taskid_set`.

- derived_task_ids:

  `character` vector of derived task IDs (variables whose values depend
  on that of other task ID variables). Defaults to NULL, meaning there
  are no derived task IDs.

- return_missing_combos:

  `boolean` specifying whether to return a `data.frame` summarizing the
  missing combinations of dependent tasks for each model. If TRUE, the
  columns of the `data.frame` will be "model_id" and one for each of the
  dependent tasks (complement of the `compound_taskid_set`). Defaults to
  FALSE.

## Value

If `model_out_tbl` passes the validations, there will be no return
value. Otherwise, the function will either throw an error if
`return_missing_combos` is FALSE, or a `data.frame` of the missing
combinations of dependent tasks will be returned. See above for more
details.
