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
#' @param comp_unit_cols `character` vector giving the compound task ID variable set.
#'   NULL means all columns' values display dependency while equality to
#'   task_id_cols means that none of the columns' values are dependent.
#'   Defaults to NA, in which case the function will pull this information from
#' @param n_output_samples `numeric` that specifies how many sample forecasts to
#'   return per unique combination of task IDs. Currently the only supported value
#'   is NULL, in which case all provided component model samples are collected and
#'   returned.
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

linear_pool <- function(model_out_tbl, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        comp_unit_cols = NA,
                        n_samples = 1e4,
                        n_output_samples = NULL,
                        ...) {

  # validate_ensemble_inputs
  valid_types <- c("mean", "quantile", "cdf", "pmf", "sample")
  validated_inputs <- model_out_tbl |>
    validate_ensemble_inputs(weights = weights,
                             weights_col_name = weights_col_name,
                             task_id_cols = task_id_cols,
                             comp_unit_cols = comp_unit_cols,
                             valid_output_types = valid_types)

  model_out_tbl_validated <- validated_inputs$model_out_tbl
  weights_validated <- validated_inputs$weights
  task_id_cols_validated <- validated_inputs$task_id_cols
  comp_unit_cols_validated <- validated_inputs$comp_unit_cols

  # calculate linear opinion pool for different types
  split_models <- split(model_out_tbl_validated,
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
                           comp_unit_cols = comp_unit_cols_validated,
                           n_output_samples = n_output_samples)
      }
    }) |>
    purrr::list_rbind() |>
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
