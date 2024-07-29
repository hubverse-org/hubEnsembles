#' Compute ensemble model outputs as a linear pool, otherwise known as a
#' distributional mixture, of component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `quantile`, `cdf`, and `pmf`.
#'
#' @inheritParams simple_ensemble
#' @param n_samples `numeric` that specifies the number of samples to use when
#'   calculating quantiles from an estimated quantile function. Defaults to `1e4`.
#' @param ... parameters that are passed to `distfromq::make_q_fn`, specifying
#'   details of how to estimate a quantile function from provided quantile levels
#'   and quantile values for `output_type` `"quantile"`.
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
#'   additional columns in the input `model_outputs` are dropped.
#'
#' @export
#'
#' @examples
#' # We illustrate the calculation of a linear pool when we have quantiles from the
#' # component models. We take the components to be normal distributions with
#' # means -3, 0, and 3, all standard deviations 1, and weights 0.25, 0.5, and 0.25.
#' library(purrr)
#' component_ids <- letters[1:3]
#' component_weights <- c(0.25, 0.5, 0.25)
#' component_means <- c(-3, 0, 3)
#'
#' lp_qs <- seq(from = -5, to = 5, by = 0.25) # linear pool quantiles, expected outputs
#' ps <- rep(0, length(lp_qs))
#' for (m in seq_len(3)) {
#'   ps <- ps + component_weights[m] * pnorm(lp_qs, mean = component_means[m])
#' }
#'
#' component_qs <- purrr::map(component_means, ~ qnorm(ps, mean=.x)) %>% unlist()
#' component_outputs <- data.frame(
#'   stringsAsFactors = FALSE,
#'   model_id = rep(component_ids, each = length(lp_qs)),
#'   target = "inc death",
#'   output_type = "quantile",
#'   output_type_id = ps,
#'   value = component_qs)
#'
#' lp_from_component_qs <- linear_pool(
#'   component_outputs,
#'   weights = data.frame(model_id = component_ids, weight = component_weights))
#'
#' head(lp_from_component_qs)
#' all.equal(lp_from_component_qs$value, lp_qs, tolerance = 1e-3,
#'           check.attributes=FALSE)
#'

linear_pool <- function(model_outputs, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        n_samples = 1e4,
                        ...) {

  # validate_ensemble_inputs
  valid_types <- c("mean", "quantile", "cdf", "pmf")
  validated_inputs <- model_outputs |>
    validate_ensemble_inputs(weights = weights,
                             weights_col_name = weights_col_name,
                             task_id_cols = task_id_cols,
                             valid_output_types = valid_types)

  model_outputs_validated <- validated_inputs$model_outputs
  weights_validated <- validated_inputs$weights
  task_id_cols_validated <- validated_inputs$task_id_cols

  # calculate linear opinion pool for different types
  ensemble_model_outputs <- model_outputs_validated |>
    dplyr::group_split("output_type") |>
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
      }
    }) |>
    purrr::list_rbind() |>
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
