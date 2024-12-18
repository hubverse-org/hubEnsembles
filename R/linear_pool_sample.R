#' Helper function for computing ensemble model outputs as a linear pool
#' (distributional mixture) of component model outputs for the `sample`
#' output type.
#'
#' @inheritParams linear_pool
#'
#' @details The resulting output type ID values are character strings, generated by
#'   a concatenation of the component model ID and initial output type ID, unless
#'   the input `model_out_tbl` is detected to have a numeric `output_type_id`
#'   column. In the latter case, a factor representation of this character string
#'   is coerced to a numeric value.
#' @noRd
#'
#' @return a `model_out_tbl` object of ensemble predictions for the `sample`
#' output type. Note that the output type ID values will not match those of the
#' input model_out_tbl but do preserve relationships across combinations of
#' task ID variables.
#'
#' @importFrom rlang .data
linear_pool_sample <- function(model_out_tbl, weights = NULL,
                               weights_col_name = "weight",
                               model_id = "hub-ensemble",
                               task_id_cols = NULL,
                               n_output_samples = NULL) {

  validate_sample_inputs(model_out_tbl, weights, weights_col_name, n_output_samples)

  num_models <- length(unique(model_out_tbl$model_id))
  samples_per_model <- model_out_tbl |>
    dplyr::group_by(dplyr::across("model_id")) |>
    dplyr::summarize(provided_n_component_samples = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select("model_id", "provided_n_component_samples") |>
    dplyr::distinct(.keep_all = TRUE)
  unique_provided_samples <- unique(samples_per_model[["provided_n_component_samples"]])

  if (is.null(weights)) {
    weights <- data.frame(
      model_id = unique(model_out_tbl$model_id),
      weight = 1 / num_models,
      stringsAsFactors = FALSE
    )
    weights_col_name <- "weight"
  }
  unique_weights <- unique(weights[[weights_col_name]])

  if (length(unique_weights) != 1 || length(unique_provided_samples) != 1 || !is.null(n_output_samples)) {
    cli::cli_abort(
      "The requested ensemble calculation doesn't satisfy all conditions:
      1) {.arg model_out_tbl} contains the same number of samples from each component model,
      2) {.arg weights} are {.val NULL} or equal for every model,
      3) {.arg n_output_samples} = {.val NULL}"
    )
  }

  model_out_tbl |>
    make_sample_indices_unique() |>
    dplyr::select(-"model_id") |>
    dplyr::mutate(model_id = model_id, .before = 1)
}


#' Make the output type ID values of sample forecasts unique for the same
#' combination of task IDs but different models
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions).
#'
#' @details The new `output_type_id` column values will follow one of two patterns,
#' depending on whether the column is detected to be numeric:
#'   1. If the output type ID is not numeric (may be a character):
#'      A concatenation of the prediction's model ID and original output type ID
#'   2. If the output type ID is numeric: A numeric representation of the above
#'      pattern rendered as a factor.
#'
#' @return a model_out_tbl object with unique output type ID values for different
#'   models but otherwise identical to the input model_out_tbl.
make_sample_indices_unique <- function(model_out_tbl) {
  numeric_output_type_ids <- is.numeric(model_out_tbl$output_type_id)

  new_indices_outputs <- model_out_tbl |>
    dplyr::mutate(output_type_id = paste0(.data[["model_id"]], .data[["output_type_id"]]))

  if (numeric_output_type_ids) {
    new_indices_outputs |>
      dplyr::mutate(output_type_id = as.integer(factor(.data[["output_type_id"]])))
  } else {
    new_indices_outputs
  }
}


#' Perform simple validations on the inputs used to calculate a linear pool
#' of samples
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions). May only contain the "sample" output type.
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have a column named `model_id` and a column containing
#'   model weights. Default to `NULL`, which specifies an equally-weighted ensemble
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`
#' @param n_output_samples `numeric` that specifies how many sample forecasts to
#'   return per unique combination of task IDs. Currently the only supported value
#'   is NULL, in which case all provided component model samples are collected and
#'   returned.
#'
#' @return no return value
#'
#' @noRd
validate_sample_inputs <- function(model_out_tbl, weights = NULL,
                                   weights_col_name = "weight",
                                   n_output_samples = NULL) {
  if (!identical(unique(model_out_tbl$output_type), "sample")) {
    cli::cli_abort("{.arg model_out_tbl} should only contain the sample output type")
  }

  if (!is.null(n_output_samples) && !is.numeric(n_output_samples) && trunc(n_output_samples) != n_output_samples) {
    cli::cli_abort("{.arg n_output_samples} must be {.val NULL} or an integer value")
  }

  if (!is.null(weights) && is.null(n_output_samples)) {
    cli::cli_abort("Component model weights output samples provided,
                   so a number of ensemble samples {.arg n_output_samples} must be provided")
  }

  if (!is.null(weights) && !all(colnames(weights) %in% c("model_id", weights_col_name))) {
    cli::cli_abort("Currently weights for different task IDs are not supported for the sample output type.")
  }
}
