#' Helper function for computing ensemble model outputs as a linear pool
#' (distributional mixture) of component model outputs for the `quantile`
#' output type.
#'
#' @inherit linear_pool params details
#' @noRd
#'
#' @return a `model_out_tbl` object of ensemble predictions for the `quantile` output type.
#' @importFrom rlang .data

linear_pool_quantile <- function(model_out_tbl, weights = NULL,
                                 weights_col_name = "weight",
                                 model_id = "hub-ensemble",
                                 task_id_cols = NULL,
                                 n_samples = 1e4,
                                 ...) {
  quantile_levels <- unique(model_out_tbl$output_type_id)

  if (is.null(weights)) {
    group_by_cols <- task_id_cols
    agg_args <- list(x = quote(.data[["pred_qs"]]),
                     weights = NULL,
                     normalize = TRUE,
                     probs = as.numeric(quantile_levels))

  } else {
    weight_by_cols <- colnames(weights)[colnames(weights) != weights_col_name]

    model_out_tbl <- model_out_tbl |>
      dplyr::left_join(weights, by = weight_by_cols)

    agg_args <- list(x = quote(.data[["pred_qs"]]),
                     weights = quote(.data[[weights_col_name]]),
                     normalize = TRUE,
                     probs = as.numeric(quantile_levels))

    group_by_cols <- c(task_id_cols, weights_col_name)
  }

  sample_q_lvls <- seq(from = 0, to = 1, length.out = n_samples + 2)[2:n_samples + 1]
  quantile_outputs <- model_out_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", group_by_cols)))) |>
    dplyr::summarize(
      pred_qs = list(
        distfromq::make_q_fn(
          ps = as.numeric(.data[["output_type_id"]]),
          qs = .data[["value"]], ...
        )(sample_q_lvls)
      ),
      .groups = "drop"
    ) |>
    tidyr::unnest("pred_qs") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(task_id_cols))) |>
    dplyr::summarize(
      output_type_id = list(quantile_levels),
      value = list(do.call("weighted_quantile", args = agg_args)),
      .groups = "drop"
    ) |>
    tidyr::unnest(cols = tidyselect::all_of(c("output_type_id", "value"))) |>
    dplyr::mutate(model_id = model_id, .before = 1) |>
    dplyr::mutate(output_type = "quantile", .before = "output_type_id") |>
    dplyr::ungroup()

  return(quantile_outputs)
}
