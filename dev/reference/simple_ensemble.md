# Compute ensemble model outputs by summarizing component model outputs for each combination of model task, output type, and output type id. Supported output types include `mean`, `median`, `quantile`, `cdf`, and `pmf`.

Compute ensemble model outputs by summarizing component model outputs
for each combination of model task, output type, and output type id.
Supported output types include `mean`, `median`, `quantile`, `cdf`, and
`pmf`.

## Usage

``` r
simple_ensemble(
  model_out_tbl,
  weights = NULL,
  weights_col_name = "weight",
  agg_fun = mean,
  agg_args = list(),
  model_id = "hub-ensemble",
  task_id_cols = NULL
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

- agg_fun:

  a function or character string name of a function to use for
  aggregating component model outputs into the ensemble outputs. See the
  details for more information.

- agg_args:

  a named list of any additional arguments that will be passed to
  `agg_fun`.

- model_id:

  `character` string with the identifier to use for the ensemble model.

- task_id_cols:

  `character` vector with names of columns in `model_out_tbl` that
  specify modeling tasks. Defaults to `NULL`, in which case all columns
  in `model_out_tbl` other than `"model_id"`, `"output_type"`,
  `"output_type_id"`, and `"value"` are used as task ids.

## Value

a `model_out_tbl` object of ensemble predictions. Note that any
additional columns in the input `model_out_tbl` are dropped.

## Details

The default for `agg_fun` is `"mean"`, in which case the ensemble's
output is the average of the component model outputs within each group
defined by a combination of values in the task id columns, output type,
and output type id. The provided `agg_fun` should have an argument `x`
for the vector of numeric values to summarize, and for weighted methods,
an argument `w` with a numeric vector of weights. If it desired to use
an aggregation function that does not accept these arguments, a wrapper
would need to be written. For weighted methods, `agg_fun = "mean"` and
`agg_fun = "median"` are translated to use
[`matrixStats::weightedMean`](https://rdrr.io/pkg/matrixStats/man/weightedMean.html)
and
[`matrixStats::weightedMedian`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html)
respectively. For
[`matrixStats::weightedMedian`](https://rdrr.io/pkg/matrixStats/man/weightedMedian.html),
the argument `interpolate` is automatically set to FALSE to circumvent a
calculation issue that results in invalid distributions.

## Examples

``` r
# Calculate a weighted median in two ways
data(model_outputs)
#> Warning: data set ‘model_outputs’ not found
data(fweights)
#> Warning: data set ‘fweights’ not found

weighted_median1 <- simple_ensemble(model_outputs, weights = fweights,
                                    agg_fun = stats::median)
weighted_median2 <- simple_ensemble(model_outputs, weights = fweights,
                                     agg_fun = matrixStats::weightedMedian)
all.equal(weighted_median1, weighted_median2)
#> [1] TRUE
```
