#' Compute ensemble model outputs as a linear pool, otherwise known as a
#' distributional mixture, of component model outputs for
#' each combination of model task, output type, and output type id. Supported
#' output types include `mean`, `quantile`, `cdf`, `pmf`, and `sample`.
#'
#' @param model_outputs an object of class `model_output_df` with component
#'   model outputs (e.g., predictions).
#' @param weights an optional `data.frame` with component model weights. If
#'   provided, it should have a column named `model_id` and a column containing
#'   model weights. Optionally, it may contain additional columns corresponding
#'   to task id variables, `output_type`, or `output_type_id`, if weights are
#'   specific to values of those variables. The default is `NULL`, in which case
#'   an equally-weighted ensemble is calculated.
#' @param weights_col_name `character` string naming the column in `weights`
#'   with model weights. Defaults to `"weight"`
#' @param model_id `character` string with the identifier to use for the
#'   ensemble model.
#' @param task_id_cols `character` vector with names of columns in
#'   `model_outputs` that specify modeling tasks. Defaults to `NULL`, in which
#'   case all columns in `model_outputs` other than `"model_id"`, the specified
#'   `output_type_col` and `output_type_id_col`, and `"value"` are used as task
#'   ids.
#' @param ... parameters that are passed to `distfromq::make_r_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels and
#'.  quantile values for `output_type` `"quantile"`.
#' @details The underlying mechanism for the computations varies for different `output_type`s.
#'   When the `output_type` is `cdf`, `pmf`, or `mean`, this function simply calls `simple_ensemble`
#'   to calculate a (weighted) mean of the component model outputs. This is the definitional
#'   calculation for the cdf or pmf of a linear pool. For the `mean` output type, this is justified by
#'   the fact that the (weighted) mean of the linear pool is the (weighted) mean of the means of the
#'   component distributions.
#'   
#'   When the `output_type` is `quantile`, we obtain the quantiles of a linear pool in three steps:
#'     1. Interpolate and extrapolate from the provided quantiles for each component model
#'        to obtain an estimate of the cdf of that distribution.
#'     2. Draw samples from the distribution for each component model. To reduce Monte
#'        Carlo variability, we use pseudo-random samples corresponding to quantiles
#'        of the estimated distribution.
#'     3. Collect the samples from all component models and extract the desired quantiles.
#'   Steps 1 and 2 in this process are performed by `distfromq::make_q_fun`.
#'
#' @return a `model_out_tbl` object of ensemble predictions. Note that
#'   any additional columns in the input `model_outputs` are dropped.
#'
#' @export
linear_pool <- function(model_outputs, weights = NULL,
                        weights_col_name = "weight",
                        model_id = "hub-ensemble",
                        task_id_cols = NULL,
                        ...) {
                                
  if (!is.data.frame(model_outputs)) {
    cli::cli_abort(c("x" = "{.arg model_outputs} must be a `data.frame`."))
  }

  if (isFALSE("model_out_tbl" %in% class(model_outputs))) {
    model_outputs <- hubUtils::as_model_out_tbl(model_outputs)
  }

  model_out_cols <- colnames(model_outputs)

  non_task_cols <- c("model_id", "output_type", "output_type_id", "value")
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

  valid_types <- c("mean", "quantile", "cdf", "pmf", "sample")
  unique_types <- unique(model_outputs[["output_type"]])
  invalid_types <- unique_types[!unique_types %in% valid_types]
  if (length(invalid_types) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg model_outputs} contains unsupported output type.",
      "!" = "Included invalid output type{?s}: {.val {invalid_types}}.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }

  # calculate linear opinion pool for different types
  ensemble_outputs1 <- ensemble_outputs2 <- ensemble_outputs3 <- NULL
  
  if (any(unique_types %in% c("mean", "cdf", "pmf"))) {
    # linear pool calculation for mean, cdf, pmf output types
    ensemble_outputs1 <- model_outputs %>%
      dplyr::filter(output_type %in% c("mean", "cdf", "pmf")) %>%
      hubEnsembles::simple_ensemble(weights = weights,
                                    weights_col_name = weights_col_names,
                                    agg_fun = "mean", agg_args = list(),
                                    model_id = model_id,
                                    task_id_cols = task_id_cols) 
  } 
  
  if (any(unique_types == "sample")) {
    # linear pool calculation for sample output type
    print("sample")
  } 
  
  if (any(unique_types == "quantile")) {
    # linear pool calculation for quantile output type
    n_samples <- 1e4
    quantile_levels <- unique(model_outputs$output_type_id)
    
    if (is.null(weights)) {
      weights_col_name <- NULL
      group_by_cols <- task_id_cols
      agg_args <- c(list(x = quote(.data[["pred_qs"]]), probs = quantile_levels))
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
      
    agg_args <- c(list(x = quote(.data[["pred_qs"]]),
                     weights = quote(.data[[weights_col_name]]),
                     normwt = TRUE,
                     probs = quantile_levels))
    
    group_by_cols <- c(task_id_cols, weights_col_name)
  }

  ensemble_outputs3 <- model_outputs |>
    dplyr::group_by(model_id, dplyr::across(dplyr::all_of(group_by_cols))) |>
      dplyr::summarize(
        pred_qs = list(distfromq::make_q_fn(
          ps = output_type_id,
          qs = value)(seq(from = 0, to = 1, length.out = n_samples + 2)[2:n_samples])),
        .groups = "drop"
      ) |>
      tidyr::unnest(pred_qs) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by_cols))) |>
    dplyr::summarize(
      output_type_id= list(quantile_levels),
      value = list(do.call(Hmisc::wtd.quantile, args = agg_args)),
      .groups = "drop") |>
      tidyr::unnest(cols = tidyselect::all_of(c("output_type_id", "value"))) |>
    dplyr::mutate(model_id = model_id, .before = 1) |>
    dplyr::mutate(output_type = "quantile", .before = output_type_id) |>
    dplyr::ungroup() |>
    dplyr::select(-all_of(weights_col_name)) 
  }
  
  ensemble_model_outputs <- ensemble_outputs1 %>%
    rbind(ensemble_outputs2, ensemble_outputs3) %>%
    hubUtils::as_model_out_tbl()

  return(ensemble_model_outputs)
}
