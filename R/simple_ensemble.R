#' Compute ensemble model outputs by summarizing component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `median`, `quantile`, `cdf`, and `pmf`.
#'
#' @param model_outputs an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions).
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have a column named `model_id` and a column containing
#'   model weights. Optionally, it may contain additional columns corresponding
#'   to task id variables, `output_type`, or `output_type_id`, if weights are
#'   specific to values of those variables. The default is `NULL`, in which case
#'   an equally-weighted ensemble is calculated.
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`
#' @param agg_fun a function or character string name of a function to use for
#'   aggregating component model outputs into the ensemble outputs. See the
#'   details for more information.
#' @param agg_args a named list of any additional arguments that will be passed
#'   to `agg_fun`.
#' @param model_id `character` string with the identifier to use for the
#'   ensemble model.
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. Defaults to `NULL`, in which
#'   case all columns in `model_outputs` other than `"model_id"`, the specified
#'   `output_type_col` and `output_type_id_col`, and `"value"` are used as task
#'   ids.
#'
#' @details The default for `agg_fun` is `"mean"`, in which case the ensemble's
#'   output is the average of the component model outputs within each group
#'   defined by a combination of values in the task id columns, output type, and
#'   output type id. The provided `agg_fun` should have an argument `x` for the
#'   vector of numeric values to summarize, and for weighted methods, an
#'   argument `w` with a numeric vector of weights. If it desired to use an
#'   aggregation function that does not accept these arguments, a wrapper
#'   would need to be written. For weighted methods, `agg_fun = "mean"` and
#'   `agg_fun = "median"` are translated to use `matrixStats::weightedMean` and
#'   `matrixStats::weightedMedian` respectively.
#'
#' @return a `model_out_tbl` object of ensemble predictions. Note that
#'   any additional columns in the input `model_outputs` are dropped.
#'
#' @export
simple_ensemble <- function(model_outputs, weights = NULL,
                            weights_col_name = "weight",
                            agg_fun = "mean", agg_args = list(),
                            model_id = "hub-ensemble",
                            task_id_cols = NULL) {

  # validate_ensemble_inputs
  valid_types <- c("mean", "median", "quantile", "cdf", "pmf")
  validated_inputs <- validate_ensemble_inputs(model_outputs, weights = weights,
                                       weights_col_name = weights_col_name,
                                       task_id_cols = task_id_cols,
                                       valid_output_types = valid_types)
  
    model_outputs_validated <- validated_inputs$model_outputs
    weights_validated <- validated_inputs$weights
    task_id_cols_validated <- validated_inputs$task_id_cols

  if (is.null(weights_validated)) {
    agg_args <- c(agg_args, list(x = quote(.data[["value"]])))
  } else {
    weight_by_cols <- 
      colnames(weights_validated)[colnames(weights_validated) != weights_col_name]

    model_outputs_validated <- model_outputs_validated %>%
      dplyr::left_join(weights_validated, by = weight_by_cols)

    if (is.character(agg_fun)) {
      if (agg_fun == "mean") {
        agg_fun <- matrixStats::weightedMean
      } else if (agg_fun == "median") {
        agg_fun <- matrixStats::weightedMedian
      }
    }

    agg_args <- c(agg_args, list(x = quote(.data[["value"]]),
                                 w = quote(.data[[weights_col_name]])))
  }

  group_by_cols <- c(task_id_cols_validated, "output_type", "output_type_id")
  ensemble_model_outputs <- model_outputs_validated %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_cols))) %>%
    dplyr::summarize(value = do.call(agg_fun, args = agg_args)) %>%
    dplyr::mutate(model_id = model_id, .before = 1) %>%
    dplyr::ungroup() %>%
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
