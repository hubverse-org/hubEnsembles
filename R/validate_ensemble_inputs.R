#' Perform simple validations on the inputs used to calculate an ensemble of
#' component model outputs for each combination of model task, output type,
#' and output type id. Valid output types should be specified by the user
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
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. Defaults to `NULL`, in which
#'   case all columns in `model_outputs` other than `"model_id"`, the specified
#'   `output_type_col` and `output_type_id_col`, and `"value"` are used as task
#'   ids.
#' @param valid_output_types `character` vector with the names of valid output
#'   types for the particular ensembling method used. See the details for more
#'   information.
#' @details If the ensembling function intended to be used is `"simple_ensemble"`,
#'   the valid output types are `mean`, `median`, `quantile`, `cdf`, and `pmf`.
#'   If the ensembling function will be `"linear_pool"`, the valid output types
#'   are `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#'
#' @return a list of validated model inputs: `model_outputs` object of class
#'   `model_output_df`, optional `weights` data frame, and `task_id_cols`
#'   character vector
#'
#' @noRd

validate_ensemble_inputs <- function(model_outputs, weights = NULL,
                                     weights_col_name = "weight",
                                     task_id_cols = NULL,
                                     valid_output_types) {

  if (!inherits(model_outputs, "model_out_tbl")) {
    model_outputs <- hubUtils::as_model_out_tbl(model_outputs)
  }

  model_out_cols <- colnames(model_outputs)

  non_task_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (is.null(task_id_cols)) {
    task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]
  } else if (!all(task_id_cols %in% model_out_cols)) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} did not have all listed task id columns
             {.val {task_id_col}}."
    ))
  }

  # check `model_outputs` has all standard columns with correct data type
  # and `model_outputs` has > 0 rows
  hubUtils::validate_model_out_tbl(model_outputs)

  unique_output_types <- unique(model_outputs[["output_type"]])
  invalid_output_types <- unique_output_types[!unique_output_types %in% valid_output_types]
  if (length(invalid_output_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} contains unsupported output type.",
      "!" = "Included invalid output type{?s}: {.val {invalid_output_types}}.",
      "i" = "Supported output types: {.val {valid_output_types}}."
    ))
  }

  # check if "cdf", "pmf", "quantile" distributions are valid
  if (any(unique_output_types %in% c("cdf", "pmf", "quantile"))) {
    validate_output_type_ids(model_outputs, task_id_cols)
  }

  if (!is.null(weights)) {
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
  }

  validated_inputs <- list(model_outputs = model_outputs,
                           weights = weights,
                           task_id_cols = task_id_cols)
  return(validated_inputs)
}
