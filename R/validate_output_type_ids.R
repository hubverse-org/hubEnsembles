#' Perform validations to check that within each group defined by a combination
#' of values for task id variables and output type, all models provided the same
#' set of output type ids. This check only applies to the `cdf`, `pmf`, and 
#' `quantile` output types to ensure the resulting distribution is valid.
#' @param model_outputs an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions).
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. 
#' @param valid_output_types `character` vector with the names of valid output 
#'   types for the particular ensembling method used. See the details for more 
#'   information.
#' @details If the ensembling function intended to be used is `"simple_ensemble"`, 
#'   the valid output types are `mean`, `median`, `quantile`, `cdf`, and `pmf`. 
#'   If the ensembling function will be `"linear_pool"`, the valid output types 
#'   are `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#' 
#' @return validated `model_outputs` object of class `model_output_df`
#'
#' @NoRd
#' 

validate_output_type_ids <- function(model_outputs, task_id_cols) {
  same_output_id <- model_outputs |>
    dplyr::filter(output_type %in% c("cdf", "pmf", "quantile")) |>
    dplyr::group_by(model_id, dplyr::across(dplyr::all_of(task_id_cols)), output_type) |>
    dplyr::summarize(output_type_id_list=list(output_type_id)) |>
    dplyr::ungroup() |>
    dplyr::group_split(dplyr::across(dplyr::all_of(task_id_cols)), output_type) |>
    purrr::map(.f = function(split_outputs) {
      length(unique(split_outputs$output_type_id_list)) == 1
    }) |>
    unlist()

  false_counter <- length(same_output_id[same_output_id == FALSE])
  if (FALSE %in% same_output_id) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} contains {.val {false_counter}} invalid distributions.",
      "i" = "Within each group defined by a combination of task id variables 
             and output type, all models must provide the same set of 
             output type ids"
     ))
  }
}