#' Perform validations to check that within each group defined by a combination
#' of values for task id variables and output type, all models provided the same
#' set of output type ids. This check only applies to the `cdf`, `pmf`, `quantile`,
#' and `sample` output types to ensure the resulting distribution is valid.
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions).
#' @param task_id_cols `character` vector with names of columns in
#'   `model_out_tbl` that specify modeling tasks.
#'
#' @return no return value
#' @noRd
#'
#' @importFrom rlang .data

validate_output_type_ids <- function(model_out_tbl, task_id_cols) {
  same_output_id <- model_out_tbl |>
    dplyr::filter(.data[["output_type"]] %in% c("cdf", "pmf", "quantile", "sample")) |>
    dplyr::group_by(dplyr::across(c(dplyr::all_of(task_id_cols), "model_id", "output_type"))) |>
    dplyr::summarize(output_type_id_list = list(sort(.data[["output_type_id"]]))) |>
    dplyr::ungroup() |>
    dplyr::group_split(dplyr::across(dplyr::all_of(c(task_id_cols, "output_type")))) |>
    purrr::map_lgl(.f = function(split_outputs) {
      length(unique(split_outputs$output_type_id_list)) == 1
    })

  false_counter <- sum(!same_output_id)
  if (false_counter != 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_out_tbl} contains {.val {false_counter}} invalid distributions.",
      "i" = "Within each group defined by a combination of task id variables
             and output type, all models must provide the same set of
             output type ids"
    ))
  }
}
