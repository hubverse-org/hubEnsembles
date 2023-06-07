#' Compute ensemble model outputs by summarizing component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `median`, `quantile`, `cdf`, and `pmf`.
#'
#' @param model_outputs an object of class `model_output_df` with component
#'   model outputs (e.g., predictions).
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have columns `model_id`, `weight`, and optionally,
#'   additional columns corresponding to task id variables, `output_type`, or
#'   `output_type_id`, if weights are specific to values of those variables. The
#'   default is `NULL`, in which case an equally-weighted ensemble is calculated.
#' @param agg_fun a function or character string name of a function to use for
#'   aggregating component model outputs into the ensemble outputs. See the
#'   details for more information.
#' @param agg_args a named list of any additional arguments that will be passed
#'   to `agg_fun`.
#' @param model_id `character` string with the identifier to use for the
#'   ensemble model.
#' @param task_id_cols, output_type_col, output_type_id_col, value_col
#'   `character` vectors with the names of the columns in `model_outputs` for
#'   the output's type, additional identifying information, and value of the
#'   model output.
#'
#' @details The default for `agg_fun` is `mean`, in which case the ensemble's
#'   output is the average of the component model outputs within each group
#'   defined by a combination of values in the task id columns, output type, and
#'   output type id. The provided `agg_fun` should have an argument `x` for the
#'   vector of numeric values to summarize, and for weighted methods, an
#'   argument `w` with a numeric vector of weights. For weighted methods,
#'  `agg_fun = "mean"` and `agg_fun = "median"` are translated to use
#'  `matrixStats::weightedMean` and `matrixStats::weightedMedian` respectively.
#'
#' @return a data.frame with columns `team_abbr`, `model_abbr`, one column for
#'   each task id variable, `output_type`, `output_id`, and `value`. Note that
#'   any additional columns in the input `model_outputs` are dropped.
simple_ensemble <- function(model_outputs, weights = NULL,
                            agg_fun = "mean", agg_args = list(),
                            model_id = "hub-ensemble",
                            task_id_cols = NULL,
                            output_type_col = "output_type",
                            output_type_id_col = "output_type_id",
                            hub_connection = NULL) {
  model_out_cols <- colnames(model_outputs)
  if (!is.data.frame(model_outputs)) {
    cli::cli_abort(c("x" = "{.arg model_outputs} must be a `data.frame`."))
  }

  non_task_cols <- c("model_id", output_type_col, output_type_id_col, "value")
  if (is.null(task_id_cols)) {
    task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]
  }

  req_col_names <- c(non_task_cols, task_id_cols)
  if (!all(req_col_names %in% model_out_cols)) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} did not have all required columns
             {.val {req_col_names}}."
    ))
  }

  ## Validations above this point to be relocated to hubUtils
  # hubUtils::validate_model_output_df(model_outputs)

  if (nrow(model_outputs) == 0) {
    cli::cli_warn(c("!" = "{.arg model_outputs} has zero rows."))
  }

  valid_types <- c("mean", "median", "quantile", "cdf", "pmf")
  unique_types <- unique(model_outputs[[output_type_col]])
  invalid_types <- unique_types[!unique_types %in% valid_types]
  if (length(invalid_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} contains unsupported output type.",
      "i" = "Included output type{?s}: {.val {invalid_types}}.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }

  if (is.null(weights)) {
    agg_args <- c(agg_args, list(x = quote(.data[["value"]])))
  } else {
    req_weight_cols <- c("model_id", "weight")
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

    invalid_cols <- weight_by_cols[!weight_by_cols %in% colnames(model_outputs)]
    if (!all(weight_by_cols %in% colnames(model_outputs))) {
      cli::cli_abort(c(
        "x" = "{.arg weights} included {length(invalid_cols)} column{?s} that
               {?was/were} not present in {.arg model_outputs}:
               {.val {invalid_cols}}"
      ))
    }

    if ("weight" %in% colnames(model_outputs)) {
      weight_col_name <- paste0("weight_", rlang::hash(colnames(model_outputs)))
      weights <- weights %>% dplyr::rename(!!weight_col_name := "weight")
    } else {
      weight_col_name <- "weight"
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
                                 w = quote(.data[[weight_col_name]])))
  }

  group_by_cols <- c(task_id_cols, output_type_col, output_type_id_col)
  ensemble_model_outputs <- model_outputs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_cols))) %>%
    dplyr::summarize(value = do.call(agg_fun, args = agg_args)) %>%
    dplyr::mutate(model_id = model_id, .before = 1) %>%
    dplyr::ungroup()

  # hubUtils::as_model_output_df(ensemble_model_outputs)

  return(ensemble_model_outputs)
}
