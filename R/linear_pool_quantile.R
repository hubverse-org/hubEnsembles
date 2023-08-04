#' Helper function for computing ensemble model outputs as a linear pool
#' (distributional mixture) of component model outputs for the `quantile`
#' output type.
#'
#' @param model_outputs an object of class `model_output_df` with component
#'   model outputs (e.g., predictions) with only a `quantile` output type.
#'   Should be pre-validated.
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have a column named `model_id` and a column containing
#'   model weights. Optionally, it may contain additional columns corresponding
#'   to task id variables, `output_type`, or `output_type_id`, if weights are
#'   specific to values of those variables. The default is `NULL`, in which case
#'   an equally-weighted ensemble is calculated. Should be pre-validated.
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`.
#' @param model_id `character` string with the identifier to use for the
#'   ensemble model.
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. Defaults to `NULL`, in which
#'   case all columns in `model_outputs` other than `"model_id"`, the specified
#'   `output_type_col` and `output_type_id_col`, and `"value"` are used as task
#'   ids. Should be pre-validated.
#' @param n_samples `numeric` that specifies the number of samples to use when
#'   calculating quantiles from an estimated quantile function. Defaults to `1e4`.
#'   Should not be smaller than `1e3`.
#' @param ... parameters that are passed to `distfromq::make_r_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels 
#'   and quantile values for `output_type` `"quantile"`.
#' @NoRd
#' @details The underlying mechanism for the computations to obtain the quantiles 
#'   of a linear pool in three steps is as follows:
#'     1. Interpolate and extrapolate from the provided quantiles for each component
#'        model to obtain an estimate of the cdf of that distribution.
#'     2. Draw samples from the distribution for each component model. To reduce Monte
#'        Carlo variability, we use pseudo-random samples corresponding to quantiles
#'        of the estimated distribution.
#'     3. Collect the samples from all component models and extract the desired quantiles.
#'   Steps 1 and 2 in this process are performed by `distfromq::make_q_fun`.
#' @return a `model_out_tbl` object of ensemble predictions for the `quantile` output type. 

linear_pool_quantile <- function(model_outputs, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        n_samples = 1e4,
                        ...) {
  
  quantile_levels <- unique(model_outputs$output_type_id)
  
  if (is.null(weights)) {
    group_by_cols <- task_id_cols
    agg_args <- c(list(x = quote(.data[["pred_qs"]]), probs = quantile_levels))
  } else {
    weight_by_cols <- colnames(weights)[colnames(weights) != weights_col_name]
    
    model_outputs <- model_outputs %>%
      dplyr::left_join(weights, by = weight_by_cols)
    
    agg_args <- c(list(x = quote(.data[["pred_qs"]]),
                     weights = quote(.data[[weights_col_name]]),
                     normwt = TRUE,
                     probs = quantile_levels))
    
    group_by_cols <- c(task_id_cols, weights_col_name)
  }

  quantile_outputs <- model_outputs |>
    dplyr::group_by(model_id, dplyr::across(dplyr::all_of(group_by_cols))) |>
    dplyr::summarize(
      pred_qs = list(distfromq::make_q_fn(
        ps = output_type_id,
        qs = value)(seq(from = 0, to = 1, length.out = n_samples + 2)[2:n_samples])),
      .groups = "drop") |>
    tidyr::unnest(pred_qs) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(task_id_cols))) |>
    dplyr::summarize(
      output_type_id= list(quantile_levels),
      value = list(do.call(Hmisc::wtd.quantile, args = agg_args)),
      .groups = "drop") |>
    tidyr::unnest(cols = tidyselect::all_of(c("output_type_id", "value"))) |>
    dplyr::mutate(model_id = model_id, .before = 1) |>
    dplyr::mutate(output_type = "quantile", .before = output_type_id) |>
    dplyr::ungroup() 
  
  return(quantile_outputs)
}