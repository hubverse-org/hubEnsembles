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
#' @param output_type_col `character` string with the name of the column in
#'   `model_outputs` that contains the output type.
#' @param output_type_id_col `character` string with the name of the column in
#'   `model_outputs` that contains the output type id.
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
#' @return a data.frame with columns `model_id`, one column for
#'   each task id variable, `output_type`, `output_id`, and `value`. Note that
#'   any additional columns in the input `model_outputs` are dropped.
#'
#' @export
simple_ensemble <- function(model_outputs, weights = NULL,
                            weights_col_name = "weight",
                            agg_fun = "mean", agg_args = list(),
                            model_id = "hub-ensemble",
                            task_id_cols = NULL,
                            output_type_col = "output_type",
                            output_type_id_col = "output_type_id") {
  if (!is.data.frame(model_outputs)) {
    cli::cli_abort(c("x" = "{.arg model_outputs} must be a `data.frame`."))
  }

  if (isFALSE("model_out_tbl" %in% class(model_outputs))) {
    model_outputs <- hubUtils::as_model_out_tbl(model_outputs)
  }

  model_out_cols <- colnames(model_outputs)

  non_task_cols <- c("model_id", output_type_col, output_type_id_col, "value")
  if (is.null(task_id_cols)) {
    task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]
  }

  if (!all(task_id_cols %in% model_out_cols)) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} did not have all listed task id columns
             {.val {task_id_col}}."
    ))
  }
  
  # check `model_outputs` has all standard columns with correct data type
  # and `model_outputs` has > 0 rows
  hubUtils::validate_model_out_tbl(model_outputs)

  valid_types <- c("mean", "median", "quantile", "cdf", "pmf")
  unique_types <- unique(model_outputs[[output_type_col]])
  invalid_types <- unique_types[!unique_types %in% valid_types]
  if (length(invalid_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} contains unsupported output type.",
      "!" = "Included invalid output type{?s}: {.val {invalid_types}}.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }

  if (is.null(weights)) {
    agg_args <- c(agg_args, list(x = quote(.data[["value"]])))
  } else {
    req_weight_cols <- c("model_id", weights_col_name)
    if (!all(req_weight_cols %in% colnames(weights))) {
      cli::cli_abort(c(
        "x" = "{.arg weights} did not include required columns
               {.val {req_weight_cols}}."
      ))
    }

    weight_by_cols <- colnames(weights)[colnames(weights) != weights_col_name]

    if ("value" %in% weight_by_cols) {
      cli::cli_abort(c(
        "x" = "{.arg weights} included a column named {.val {\"value\"}},
               which is not allowed."
      ))
    }

    invalid_cols <- weight_by_cols[!weight_by_cols %in% colnames(model_outputs)]
    if (length(invalid_cols) > 0) {
      cli::cli_abort(c(
        "x" = "{.arg weights} included {length(invalid_cols)} column{?s} that
               {?was/were} not present in {.arg model_outputs}:
               {.val {invalid_cols}}"
      ))
    }

    if (weights_col_name %in% colnames(model_outputs)) {
      cli::cli_abort(c(
        "x" = "The specified {.arg weights_col_name}, {.val {weights_col_name}},
               is already a column in {.arg model_outputs}."
      ))
    }

    model_outputs <- model_outputs %>%
      dplyr::left_join(weights, by = weight_by_cols)

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

  group_by_cols <- c(task_id_cols, output_type_col, output_type_id_col)
  ensemble_model_outputs <- model_outputs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_cols))) %>%
    dplyr::summarize(value = do.call(agg_fun, args = agg_args)) %>%
    dplyr::mutate(model_id = model_id, .before = 1) %>%
    dplyr::ungroup() %>%
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
