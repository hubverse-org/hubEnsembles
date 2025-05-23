#' Compute ensemble model outputs as a linear pool, otherwise known as a
#' distributional mixture, of component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#'
#' @inheritParams simple_ensemble
#' @param n_samples `numeric` that specifies the number of samples to use when
#'   calculating quantiles from an estimated quantile function. Defaults to `1e4`.
#' @param ... parameters that are passed to `distfromq::make_q_fn`, specifying
#'   details of how to estimate a quantile function from provided quantile levels
#'   and quantile values for `output_type` `"quantile"`.
#' @param compound_taskid_set `character` vector of the compound task ID variable
#'   set. This argument is only relevant for `output_type` `"sample"`. Can be one
#'   of three possible values, with the following meanings:
#'   - `NA`: the compound_taskid_set is not relevant for the current modeling task
#'   - `NULL`: samples are from a multivariate joint distribution across all levels
#'     of all task id variables
#'   - Equality to `task_id_cols`: samples are from separate univariate distributions
#'     for each individual prediction task
#'
#'   Defaults to NA. Derived task ids must be included if all of the task ids their
#'   values depend on are part of the `compound_taskid_set`.
#' @param derived_task_ids `character` vector of derived task IDs (variables whose
#'   values depend on that of other task ID variables). Defaults to NULL, meaning
#'   there are no derived task IDs.
#' @param n_output_samples `numeric` that specifies how many sample forecasts to
#'   return per unique combination of task IDs. Currently the only supported value
#'   is NULL, in which case all provided component model samples are collected and
#'   returned.
#' @param derived_tasks `r lifecycle::badge("deprecated")` Use `derived_task_ids`
#'   instead. A `character` vector of derived task IDs.
#'
#' @details The underlying mechanism for the computations varies for different
#'   `output_type`s. When the `output_type` is `cdf`, `pmf`, or `mean`, this
#'   function simply calls `simple_ensemble` to calculate a (weighted) mean of the
#'   component model outputs. This is the definitional calculation for the CDF or
#'   PMF of a linear pool. For the `mean` output type, this is justified by the fact
#'   that the (weighted) mean of the linear pool is the (weighted) mean of the means
#'   of the component distributions.
#'
#' When the `output_type` is `quantile`, we obtain the quantiles of a linear pool
#' in three steps:
#'   1. Interpolate and extrapolate from the provided quantiles for each component
#'      model to obtain an estimate of the CDF of that distribution.
#'   2. Draw samples from the distribution for each component model. To reduce
#'      Monte Carlo variability, we use quasi-random samples corresponding to
#'      quantiles of the estimated distribution.
#'   3. Collect the samples from all component models and extract the desired
#'      quantiles.
#'
#' Steps 1 and 2 in this process are performed by `distfromq::make_q_fn`.
#'
#' When the `output_type` is `sample`, we obtain the resulting linear pool by
#' collecting samples from each model, updating the `output_type_id` values to be
#' unique for predictions that are not joint across, and pooling them together.
#' If there is a restriction on the number of samples to output per compound unit,
#' this number is divided evenly among the models for that compound unit (with any
#' remainder distributed randomly).
#'
#' @return a `model_out_tbl` object of ensemble predictions. Note that any
#'   additional columns in the input `model_out_tbl` are dropped.
#'
#' @export
#'
#' @examples
#' # We illustrate the calculation of a linear pool when we have quantiles from the
#' # component models. We take the components to be normal distributions with
#' # means -3, 0, and 3, all standard deviations 1, and weights 0.25, 0.5, and 0.25.
#' data(component_outputs)
#' data(weights)
#'
#' expected_quantiles <- seq(from = -5, to = 5, by = 0.25)
#' lp_from_component_qs <- linear_pool(component_outputs, weights)
#'
#' head(lp_from_component_qs)
#' all.equal(lp_from_component_qs$value, expected_quantiles, tolerance = 1e-2,
#'           check.attributes = FALSE)
#'
#' @importFrom lifecycle deprecated
linear_pool <- function(model_out_tbl, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        compound_taskid_set = NA,
                        derived_task_ids = NULL,
                        n_samples = 1e4,
                        n_output_samples = NULL,
                        ...,
                        derived_tasks = lifecycle::deprecated()) {
  # detect and warn for usage of `derived_tasks`
  if (lifecycle::is_present(derived_tasks)) {
    lifecycle::deprecate_warn("1.0.0", "linear_pool(derived_tasks)", "linear_pool(derived_task_ids)")
    derived_task_ids <- derived_tasks
  }

  # validate_ensemble_inputs
  valid_types <- c("mean", "quantile", "cdf", "pmf", "sample")
  validated_inputs <- validate_ensemble_inputs(
    model_out_tbl,
    weights = weights,
    weights_col_name = weights_col_name,
    task_id_cols = task_id_cols,
    compound_taskid_set = compound_taskid_set,
    derived_task_ids = derived_task_ids,
    n_output_samples = n_output_samples,
    valid_output_types = valid_types
  )

  model_out_tbl_validated <- validated_inputs$model_out_tbl
  weights_validated <- validated_inputs$weights
  task_id_cols_validated <- validated_inputs$task_id_cols
  compound_taskid_set_validated <- validated_inputs$compound_taskid_set

  # calculate linear opinion pool for different types
  split_models <- split(
    model_out_tbl_validated,
    f = model_out_tbl_validated$output_type
  )
  ensemble_model_outputs <- split_models |>
    purrr::map(.f = function(split_outputs) {
      type <- split_outputs$output_type[1]
      if (type %in% c("mean", "cdf", "pmf")) {
        simple_ensemble(split_outputs, weights = weights_validated,
                        weights_col_name = weights_col_name,
                        agg_fun = "mean", agg_args = list(),
                        model_id = model_id,
                        task_id_cols = task_id_cols_validated)
      } else if (type == "quantile") {
        linear_pool_quantile(split_outputs, weights = weights_validated,
                             weights_col_name = weights_col_name,
                             model_id = model_id,
                             n_samples = n_samples,
                             task_id_cols = task_id_cols_validated,
                             ...)
      } else if (type == "sample") {
        linear_pool_sample(split_outputs, weights = weights_validated,
                           weights_col_name = weights_col_name,
                           model_id = model_id,
                           task_id_cols = task_id_cols_validated,
                           compound_taskid_set = compound_taskid_set_validated,
                           n_output_samples = n_output_samples)
      }
    }) |>
    purrr::list_rbind() |>
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
