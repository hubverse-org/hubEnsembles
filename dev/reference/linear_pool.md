# Compute ensemble model outputs as a linear pool, otherwise known as a distributional mixture, of component model outputs for each combination of model task, output type, and output type id. Supported output types include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.

Compute ensemble model outputs as a linear pool, otherwise known as a
distributional mixture, of component model outputs for each combination
of model task, output type, and output type id. Supported output types
include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.

## Usage

``` r
linear_pool(
  model_out_tbl,
  weights = NULL,
  weights_col_name = "weight",
  model_id = "hub-ensemble",
  task_id_cols = NULL,
  compound_taskid_set = NA,
  derived_task_ids = NULL,
  n_samples = 10000,
  n_output_samples = NULL,
  ...,
  derived_tasks = lifecycle::deprecated()
)
```

## Arguments

- model_out_tbl:

  an object of class `model_out_tbl` with component model outputs (e.g.,
  predictions).

- weights:

  an optional `data.frame` with component model weights. If provided, it
  should have a column named `model_id` and a column containing model
  weights. Optionally, it may contain additional columns corresponding
  to task id variables, `output_type`, or `output_type_id`, if weights
  are specific to values of those variables. The default is `NULL`, in
  which case an equally-weighted ensemble is calculated. Should be
  prevalidated.

- weights_col_name:

  `character` string naming the column in `weights` with model weights.
  Defaults to `"weight"`

- model_id:

  `character` string with the identifier to use for the ensemble model.

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

- n_samples:

  `numeric` that specifies the number of samples to use when calculating
  quantiles from an estimated quantile function. Defaults to `1e4`.

- n_output_samples:

  `numeric` that specifies how many sample forecasts to return per
  unique combination of task IDs. Cannot exceed the number of provided
  samples per compound unit (defined by the compound task id set).
  Defaults to NULL, in which case all provided component model samples
  are collected and returned.

- ...:

  parameters that are passed to
  [`distfromq::make_q_fn`](http://reichlab.io/distfromq/reference/make_q_fn.md),
  specifying details of how to estimate a quantile function from
  provided quantile levels and quantile values for `output_type`
  `"quantile"`.

- derived_tasks:

  **\[deprecated\]** Use `derived_task_ids` instead. A `character`
  vector of derived task IDs.

## Value

a `model_out_tbl` object of ensemble predictions. Note that any
additional columns in the input `model_out_tbl` are dropped.

## Details

The underlying mechanism for the computations varies for different
`output_type`s. When the `output_type` is `cdf`, `pmf`, or `mean`, this
function simply calls `simple_ensemble` to calculate a (weighted) mean
of the component model outputs. This is the definitional calculation for
the CDF or PMF of a linear pool. For the `mean` output type, this is
justified by the fact that the (weighted) mean of the linear pool is the
(weighted) mean of the means of the component distributions.

When the `output_type` is `quantile`, we obtain the quantiles of a
linear pool in three steps:

1.  Interpolate and extrapolate from the provided quantiles for each
    component model to obtain an estimate of the CDF of that
    distribution.

2.  Draw samples from the distribution for each component model. To
    reduce Monte Carlo variability, we use quasi-random samples
    corresponding to quantiles of the estimated distribution.

3.  Collect the samples from all component models and extract the
    desired quantiles.

Steps 1 and 2 in this process are performed by
[`distfromq::make_q_fn`](http://reichlab.io/distfromq/reference/make_q_fn.md).

When the `output_type` is `sample`, we obtain the resulting linear pool
by collecting samples from each model, updating the `output_type_id`
values to be unique for predictions that are not joint across, and
pooling them together. If there is a restriction on the number of
samples to output per compound unit, this number is divided evenly among
the models for that compound unit (with any remainder distributed
randomly).

## Examples

``` r
# We illustrate the calculation of a linear pool when we have quantiles from the
# component models. We take the components to be normal distributions with
# means -3, 0, and 3, all standard deviations 1, and weights 0.25, 0.5, and 0.25.
data(component_outputs)
#> Warning: data set ‘component_outputs’ not found
data(weights)
#> Warning: data set ‘weights’ not found

expected_quantiles <- seq(from = -5, to = 5, by = 0.25)
lp_from_component_qs <- linear_pool(component_outputs, weights)

head(lp_from_component_qs)
#> # A tibble: 6 × 5
#>   model_id     target    output_type output_type_id value
#>   <chr>        <chr>     <chr>                <dbl> <dbl>
#> 1 hub-ensemble inc death quantile           0.00569 -5.00
#> 2 hub-ensemble inc death quantile           0.0100  -4.75
#> 3 hub-ensemble inc death quantile           0.0167  -4.50
#> 4 hub-ensemble inc death quantile           0.0264  -4.25
#> 5 hub-ensemble inc death quantile           0.0397  -4.00
#> 6 hub-ensemble inc death quantile           0.0567  -3.75
all.equal(lp_from_component_qs$value, expected_quantiles, tolerance = 1e-2,
          check.attributes = FALSE)
#> [1] TRUE
```
