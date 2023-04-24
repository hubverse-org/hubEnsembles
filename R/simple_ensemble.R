#' Compute ensemble predictions by summarizing component predictions for each
#' combination of model task, output type, and type id. Supported output types
#' include `mean`, `median`, `quantile`, `cdf`, and `category`.
#'
#' @param predictions a `data.frame` with component model predictions. It should
#'   have columns `team_abbr`, `model_abbr`, one column for each task id
#'   variable, `output_type`, `output_type_id`, and `value`.
#' @param task_id_vars an optional `character` vector naming the columns of
#'   `predictions` that correspond to task id variables. The default is `NULL`,
#'   in which case the task id variables are looked up from the `hub_con` if one
#'   is provided. If neither `task_id_vars` nor `hub_con` are provided, all
#'   columns in `predictions` _other than_ `team_abbr`, `model_abbr`,
#'   `output_type`, `output_id`, and `value` will be used as task id
#'   variables.
#' @param hub_con an optional hub connection object; see `hubUtils::connect_hub`
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
#'   argument `x` for the vector of numeric values to summarize, and for weighted
#'   methods, an argument `w` with a numeric vector of weights.
#' @param agg_args a named list of any additional arguments that will be passed
#'   to `agg_fun`.
#' @param team_abbr, model_abbr `character` strings with the name of the team
#'   and model to use for the ensemble predictions.
#'
#' @return a data.frame with columns `team_abbr`, `model_abbr`, one column for
#'   each task id variable, `output_type`, `output_id`, and `value`.

simple_ensemble <- function(predictions, task_id_vars = NULL, hub_con = NULL,
                     weights = NULL, agg_fun = mean, agg_args = list(),
                     team_abbr = "Hub", model_abbr = "ensemble") {

  # require(matrixStats)
    
  if (is.null(task_id_vars) && is.null(hub_con)) {
    temp <- colnames(predictions)
    task_id_vars <- temp[!temp %in% c("team_abbr", "model_abbr", "output_type", "output_id", "value")]
  } else if (is.null(task_id_vars)) {
    # task_id variables looked up from `hub_con`
  }
  
  col_names <- c("team_abbr", "model_abbr", task_id_vars, "output_type", "output_id", "value")
  if ((length(predictions) == 0) || !all(names(predictions) %in% col_names)) {
    stop("predictions did not have required columns", call. = FALSE)
  } else if (!all(names(predictions) == col_names) && names(predictions) %in% col_names) {
    predictions <- relocate(predictions, all_of(col_names))
  }
  
  if (!all(predictions$output_type %in% c("mean", "median", "quantile", "cdf", "category"))) stop("Predictions contains unsupported output type") # throw warning or error

  if (is.null(weights)) {
    if (agg_fun == "mean") agg_fun = "mean"
    if (agg_fun == "median") agg_fun = "median"
    
    ensemble_predictions <- predictions %>%
      dplyr::group_by(across(all_of(c(task_id_vars, "output_type", "output_id")))) %>%
      dplyr::summarize(value = do.call(agg_fun, args = c(agg_args, list(x=value)))) %>%
      dplyr::mutate(team_abbr = team_abbr, model_abbr = model_abbr, .before = 1)
      # do we want to have the horizon column before target?
  } else {
    if (!all(names(weights) %in% c("team_abbr", "model_abbr", "weight"))) {
      stop("weights did not have required columns", call. = FALSE)
    }
    
    if (agg_fun == "mean") agg_fun = "weightedMean"
    if (agg_fun == "median") agg_fun = "weightedMedian"
    
    ensemble_predictions <- predictions %>%
      dplyr::left_join(weights) %>%
      dplyr::group_by(across(all_of(c(task_id_vars, "output_type", "output_id")))) %>%
      dplyr::summarize(value = do.call(agg_fun, args = c(agg_args, list(x=value, w=weights)))) %>%
      dplyr::mutate(team_abbr = team_abbr, model_abbr = model_abbr, .before = 1)
      # do we want to have the horizon column before target?
  }  
    
  return (ensemble_predictions)
}
