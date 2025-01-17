#' Perform simple validations on the inputs used to calculate an ensemble of
#' component model outputs for each combination of model task, output type,
#' and output type id. Valid output types should be specified by the user
#'
#' @inheritParams linear_pool
#' @param valid_output_types `character` vector with the names of valid output
#'   types for the particular ensembling method used. See the details for more
#'   information.
#'
#' @details If the ensembling function intended to be used is `"simple_ensemble"`,
#'   the valid output types are `mean`, `median`, `quantile`, `cdf`, and `pmf`.
#'   If the ensembling function will be `"linear_pool"`, the valid output types
#'   are `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#'
#' @return a list of validated model inputs: `model_out_tbl` object of class
#'   `model_output_df`, optional `weights` data frame, and `task_id_cols`
#'   character vector
#'
#' @noRd

validate_ensemble_inputs <- function(model_out_tbl, weights = NULL,
                                     weights_col_name = "weight",
                                     task_id_cols = NULL,
                                     compound_taskid_set = NA,
                                     derived_tasks = NULL,
                                     valid_output_types) {

  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  model_out_cols <- colnames(model_out_tbl)

  non_task_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (is.null(task_id_cols)) {
    task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]
  } else if (!all(task_id_cols %in% model_out_cols)) {
    cli::cli_abort(c(
      "x" = "{.arg model_out_tbl} did not have all listed task id columns
             {.val {task_id_cols}}."
    ))
  }

  # check `model_out_tbl` has all standard columns with correct data type
  # and `model_out_tbl` has > 0 rows
  hubUtils::validate_model_out_tbl(model_out_tbl)

  unique_output_types <- unique(model_out_tbl[["output_type"]])
  invalid_output_types <- unique_output_types[!unique_output_types %in% valid_output_types]
  if (length(invalid_output_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_out_tbl} contains unsupported output type.",
      "!" = "Included invalid output type{?s}: {.val {invalid_output_types}}.",
      "i" = "Supported output types: {.val {valid_output_types}}."
    ))
  }

  if ("sample" %in% unique_output_types) {
    validate_compound_taskid_set(model_out_tbl,
                                 task_id_cols, compound_taskid_set, derived_tasks,
                                 return_missing_combos = FALSE)
  }

  # check if "cdf", "pmf", "quantile" distributions are valid
  if (any(unique_output_types %in% c("cdf", "pmf", "quantile"))) {
    validate_output_type_ids(model_out_tbl, task_id_cols)
  }

  if (!is.null(weights)) {
    validate_weights(model_out_cols, weights, weights_col_name)

    if (any(c("cdf", "pmf") %in% unique_output_types) && "output_type_id" %in% colnames(weights)) {
      # nolint start
      cdf_pmf_types <- unique_output_types[unique_output_types %in% c("cdf", "pmf")]
      # nolint end
      cli::cli_abort(c(
        "x" = "{.args weights} contains weights dependent on the output type id, 
               but {.arg model_out_tbl} contains {.val {cdf_pmf_types}} forecasts.",
        "i" = "This may lead to an invalid ensemble distribution."
      ))
    }
  }

  validated_inputs <- list(model_out_tbl = model_out_tbl,
                           weights = weights,
                           task_id_cols = task_id_cols,
                           compound_taskid_set = compound_taskid_set)
  return(validated_inputs)
}


#' Perform basic validations on the model weights used to calculate an ensemble of
#' component model outputs for each combination of model task, output type,
#' and output type id.
#'
#' @param model_out_cols `character` string naming columns in a `model_out_tbl`
#'   object of component predictions that will be ensembled using the model weights
#'   in `weights`
#' @param weights a `data.frame` of component model weights to be validated. It must
#'   contain a `model_id` column and a column giving weights, but may also contain
#'   additional columns corresponding to task id variables, `output_type`, or
#'   `output_type_id`, if weights are specific to values of those variables.
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`
#'
#' @return no return value
#' @noRd

validate_weights <- function(model_out_cols, weights = NULL, weights_col_name = "weight") {
  req_weight_cols <- c("model_id", weights_col_name)
  if (!all(req_weight_cols %in% colnames(weights))) {
    cli::cli_abort(c(
      "x" = "{.arg weights} did not include required columns {.val {req_weight_cols}}."
    ))
  }

  weight_by_cols <- colnames(weights)[colnames(weights) != weights_col_name]

  if ("value" %in% weight_by_cols) {
    cli::cli_abort(c(
      "x" = "{.arg weights} included a column named {.val {\"value\"}}, which is not allowed."
    ))
  }

  invalid_cols <- weight_by_cols[!weight_by_cols %in% model_out_cols]
  if (length(invalid_cols) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg weights} included {length(invalid_cols)} column{?s} that
             {?was/were} not present in {.arg model_out_tbl}: {.val {invalid_cols}}"
    ))
  }

  if (weights_col_name %in% model_out_cols) {
    cli::cli_abort(c(
      "x" = "The specified {.arg weights_col_name}, {.val {weights_col_name}},
             is already a column in {.arg model_out_tbl}."
    ))
  }
}


#' Perform validations on the compound task ID set used to calculate an ensemble of
#' component model outputs for the sample output type, including checks that
#' (1) `compound_taskid_set` is a subset of `task_id_cols`, (2) the provided
#' `model_out_tbl` is compatible with the specified `compound_taskid_set`, and
#' (3) all models submit predictions for the same set of non `compound_taskid_set`
#' variables.
#'
#' @inheritParams linear_pool
#' @param return_missing_combos `boolean` specifying whether to return a `data.frame`
#'   summarizing the missing combinations of dependent tasks for each model. If
#'   TRUE, the columns of the `data.frame` will be "model_id" and one for each of the
#'   dependent tasks (complement of the `compound_taskid_set`). Defaults to FALSE.
#'
#' @return If `model_out_tbl` passes the validations, there will be no return value.
#'   Otherwise, the function will either throw an error if `return_missing_combos` is
#'   FALSE, or a `data.frame` of the missing combinations of dependent tasks will be
#'   returned. See above for more details.
validate_compound_taskid_set <- function(model_out_tbl,
                                         task_id_cols, compound_taskid_set, derived_tasks = NULL,
                                         return_missing_combos = FALSE) {
  if (identical(compound_taskid_set, NA)) {
    cli::cli_abort("{.arg compound_taskid_set} must be provided if 
      {.arg model_out_tbl} contains the sample output type")
  }

  if (!all(compound_taskid_set %in% task_id_cols)) {
    cli::cli_abort(
      "{.arg compound_taskid_set} contains columns not in {.arg task_id_cols}: 
      {.val {setdiff(compound_taskid_set, task_id_cols)}}"
    )
  }

  # the set of "non-derived" task id variables for which the samples should capture
  # dependence. It is sufficient to check for dependence for just these variables
  # below, since if indexing is correct for these variables, it will also be correct
  # for any task ids that are derived from them.
  dependent_tasks <- setdiff(task_id_cols, c(compound_taskid_set, derived_tasks))

  # check component model outputs are compatible with the specified compound task id set vars.
  # output_type_id levels (i.e., sample indices) must be shared across all combinations of
  # non-compound task id set vars, indicating a sample from a joint distribution across
  # those variables.
  same_output_type_ids_by_model <- model_out_tbl |>
    dplyr::filter(.data[["output_type"]] == "sample") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", dependent_tasks)))) |>
    dplyr::summarize(output_type_id_list = list(sort(.data[["output_type_id"]]))) |>
    dplyr::ungroup() |>
    dplyr::group_split(dplyr::across(dplyr::all_of(c("model_id")))) |>
    purrr::map_lgl(.f = function(split_outputs) {
      length(unique(split_outputs$output_type_id_list)) == 1
    })

  false_counter <- sum(!same_output_type_ids_by_model)
  if (false_counter != 0) {
    cli::cli_abort(c(
      "x" = "The specified {.arg compound_taskid_set} is incompatible with {.val {false_counter}} 
        component models in the provided {.arg model_out_tbl}",
      "i" = "Each model's output type ID levels must be shared across all 
        combinations of non-compound task id set variables"
    ))
  }

  # check all component models forecast the same set of non-compound task id set vars
  sample_actual <- model_out_tbl |>
    dplyr::filter(.data[["output_type"]] == "sample") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", dependent_tasks)))) |>
    dplyr::summarize(unique_forecasts = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(!!!rlang::syms(c("model_id", dependent_tasks))) |>
    dplyr::tibble()
  sample_expected <- sample_actual |>
    tidyr::complete(
      !!!rlang::syms(c("model_id", dependent_tasks)),
      fill = list(unique_forecasts = 0),
      explicit = TRUE
    ) |>
    dplyr::arrange(!!!rlang::syms(c("model_id", dependent_tasks)))

  if (!identical(sample_actual, sample_expected)) {
    if (return_missing_combos) {
      model_out_tbl |>
        tidyr::complete(
          !!!rlang::syms(c("model_id")),
          tidyr::nesting(!!!rlang::syms(setdiff(task_id_cols, derived_tasks))),
        ) |>
        dplyr::filter(is.na(.data[["output_type"]])) |>
        dplyr::arrange(!!!rlang::syms(c("model_id", dependent_tasks))) |>
        dplyr::select(dplyr::all_of(c("model_id", setdiff(task_id_cols, derived_tasks))))
    } else {
      cli::cli_abort(c(
        x = "Not all component models in {.arg model_out_tbl} forecast for the same set of dependent tasks",
        i = "Run {.arg validate_compound_taskid_set(model_out_tbl, task_id_cols, 
          compound_taskid_set, return_missing_combos = TRUE)} to see the missing dependent tasks"
      ))
    }
  }
}
