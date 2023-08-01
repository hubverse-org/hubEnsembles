#' Compute ensemble model outputs as a linear pool, otherwise known as a
#' distributional mixture, of component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#'
#' @param model_outputs an object of class `model_output_df` with component
#'   model outputs (e.g., predictions).
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have a column named `model_id` and a column containing
#'   model weights. Optionally, it may contain additional columns corresponding
#'   to task id variables, `output_type`, or `output_type_id`, if weights are
#'   specific to values of those variables. The default is `NULL`, in which case
#'   an equally-weighted ensemble is calculated.
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`
#' @param model_id `character` string with the identifier to use for the
#'   ensemble model.
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. Defaults to `NULL`, in which
#'   case all columns in `model_outputs` other than `"model_id"`, the specified
#'   `output_type_col` and `output_type_id_col`, and `"value"` are used as task
#'   ids.
#' @param ... parameters that are passed to `distfromq::make_r_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels and
#'.  quantile values for `output_type` `"quantile"`.
#' @details The underlying mechanism for the computations varies for different `output_type`s.
#'   When the `output_type` is `cdf`, `pmf`, or `mean`, this function simply calls `simple_ensemble`
#'   to calculate a (weighted) mean of the component model outputs. This is the definitional
#'   calculation for the cdf or pmf of a linear pool. For the `mean` output type, this is justified by
#'   the fact that the (weighted) mean of the linear pool is the (weighted) mean of the means of the
#'   component distributions.
#'   
#'   When the `output_type` is `quantile`, we obtain the quantiles of a linear pool in three steps:
#'     1. Interpolate and extrapolate from the provided quantiles for each component model
#'        to obtain an estimate of the cdf of that distribution.
#'     2. Draw samples from the distribution for each component model. To reduce Monte
#'        Carlo variability, we use pseudo-random samples corresponding to quantiles
#'        of the estimated distribution.
#'     3. Collect the samples from all component models and extract the desired quantiles.
#'   Steps 1 and 2 in this process are performed by `distfromq::make_q_fun`.
#'
#' @return a `model_out_tbl` object of ensemble predictions. Note that
#'   any additional columns in the input `model_outputs` are dropped.
#'
#' @export
linear_pool <- function(model_outputs, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        ...) {

  # validate_ensemble_inputs
  valid_types <- c("mean", "quantile", "cdf", "pmf", "sample")
  validated_inputs <- validate_ensemble_inputs(model_outputs, weights=weights,
                                       weights_col_name = weights_col_name,
                                       task_id_cols = task_id_cols,
                                       valid_output_types = valid_types)
  
  model_outputs_validated <- validated_inputs$model_outputs
  weights_validated <- validated_inputs$weights
  task_id_cols_validated <- validated_inputs$task_id_cols
  
  # calculate linear opinion pool for different types
  ensemble_model_outputs <- model_outputs_validated |>
    dplyr::group_split(output_type) |>
    purrr::map_dfr(.f = function(split_outputs) {
      type <- base::unique(split_outputs$output_type)
      if (type %in% c("mean", "cdf", "pmf")) {
        simple_ensemble(split_outputs, weights = weights_validated,
                              weights_col_name = weights_col_name,
                              agg_fun = "mean", agg_args = list(),
                              model_id = model_id,
                              task_id_cols = task_id_cols_validated) 
      } else if (type == "sample") {
        # to be written
      } else if (type == "quantile"){
        linear_pool_quantile(split_outputs, weights = weights_validated,
                              weights_col_name = weights_col_name,
                              model_id = model_id,
                              n_samples = 1e4,
                              task_id_cols = task_id_cols_validated) 
      }
    }) |>
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
