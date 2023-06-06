#' Compute ensemble predictions by summarizing component predictions for each
#' combination of model task, output type, and output type id. Supported output
#' types include `mean`, `median`, `quantile`, `cdf`, and `pmf`.
#'
#' @param predictions a `data.frame` with component model predictions. It should
#'   have columns `team_abbr`, `model_abbr`, one column for each task id
#'   variable, `output_type`, `output_type_id`, and `value`.
#' @param task_id_cols an optional `character` vector naming the columns of
#'   `predictions` that correspond to task id variables. The default is `NULL`,
#'   in which case all columns in `predictions` _other than_ `team_abbr`,
#'   `model_abbr`, and the specified `output_type_col`, `output_id_col`, and
#'   `value_col` are used as task id variables.
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have columns `team_name`, `model_abbr`, `weight`,
#'   and optionally, additional columns corresponding to task id variables,
#'   `output_type`, or `output_id`, if weights are specific to values of
#'   those variables. The default is `NULL`, in which case an equally-weighted
#'   ensemble is calculated.
#' @param agg_fun a function or character string name of a function to use for
#'   aggregating component predictions into the ensemble prediction. The default
#'   is `mean`, in which case the ensemble prediction is the simple average of
#'   the component model predictions. The provided function should have an
#'   argument `x` for the vector of numeric values to summarize, and for
#'   weighted methods, an argument `w` with a numeric vector of weights.
#' @param agg_args a named list of any additional arguments that will be passed
#'   to `agg_fun`.
#' @param team_abbr, model_abbr `character` strings with the name of the team
#'   and model to use for the ensemble predictions.
#' @param output_type_col, output_id_col, value_col `character` strings with the
#'   names of the columns in `predictions` for the output's type, additional
#'   identifying information, and value (of the prediction)
#'
#' @return a data.frame with columns `team_abbr`, `model_abbr`, one column for
#'   each task id variable, `output_type`, `output_id`, and `value`.
simple_ensemble <- function(predictions, weights = NULL,
                            agg_fun = "mean", agg_args = list(),
                            team_abbr = "hub", model_abbr = "ensemble",
                            task_id_cols = NULL,
                            output_type_col = "output_type",
                            output_id_col = "output_type_id",
                            hub_connection = NULL) {
  if (!is.data.frame(predictions)) {
    cli::cli_abort(c("x" = "{.arg predictions} must be a `data.frame`."))
  }

  if (is.null(task_id_cols)) {
    cols <- colnames(predictions)
    non_task_cols <- c("team_abbr", "model_abbr", output_type_col,
                       output_id_col, "value")
    task_id_cols <- cols[!cols %in% non_task_cols]
  }

  col_names <- c("team_abbr", "model_abbr", task_id_cols, output_type_col,
                 output_id_col, "value")
  if (!all(colnames(predictions) %in% col_names)) {
    cli::cli_abort(c(
      "x" = "{.arg predictions} did not have all required columns
             {.val {col_names}}."
    ))
  }
  
  ## Validations above this point to be relocated to hubUtils

  if (nrow(predictions) == 0) {
    cli::cli_warn(c("!" = "{.arg predictions} has zero rows."))
  }

  valid_types <- c("mean", "median", "quantile", "cdf", "pmf")
  unique_types <- unique(predictions[[output_type_col]])
  invalid_types <- unique_types[!unique_types %in% valid_types]
  if (length(invalid_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg predictions} contains unsupported output type.",
      "i" = "Included output type{?s}: {.val {invalid_types}}.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }

  if (is.null(weights)) {
    if (agg_fun == "mean") agg_fun <- mean
    if (agg_fun == "median") agg_fun <- median

    agg_args <- c(agg_args, list(x = "value"))
  } else {
    req_weight_cols <- c("model_abbr", "team_abbr", "weight")
    if (!all(req_weight_cols %in% colnames(weights))) {
      cli::cli_abort(c(
        "x" = "{.arg weights} did not include required columns
               {.val {req_weight_cols}}."
      ))
    }

    weight_by_cols <- colnames(weights)[colnames(weights) != "weight"]

    if ("value" %in% weight_by_cols) {
      cli::cli_abort(c(
        "x" = "{.arg weights} included a column named {.val {\"value\"}},
               which is not allowed."
      ))
    }

    invalid_cols <- weight_by_cols[!weight_by_cols %in% colnames(predictions)]
    if (!all(weight_by_cols %in% colnames(predictions))) {
      cli::cli_abort(c(
        "x" = "{.arg weights} included {length(invalid_cols)} column{?s} that
               {?was/were} not present in {.arg predictions}:
               {.val {invalid_cols}}"
      ))
    }

    if ("weight" %in% colnames(predictions)) {
      weight_col_name <- paste0("weight_", rlang::hash(colnames(predictions)))
      weights <- weights %>% dplyr::rename(!!weight_col_name := "weight")
    } else {
      weight_col_name <- "weight"
    }

    predictions <- predictions %>%
      dplyr::left_join(weights, by = weight_by_cols)

    if (agg_fun == "mean") agg_fun <- matrixStats::weightedMean
    if (agg_fun == "median") agg_fun <- matrixStats::weightedMedian

    agg_args <- c(agg_args, list(x = "value", w = weight_col_name))
  }

  ensemble_predictions <- predictions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(task_id_cols, output_type_col,
                                                  output_id_col)))) %>%
    dplyr::summarize(value = do.call(agg_fun, args = agg_args)) %>%
    dplyr::mutate(team_abbr = team_abbr, model_abbr = model_abbr,
                  .before = 1) %>%
    dplyr::ungroup()

  return(ensemble_predictions)
}
