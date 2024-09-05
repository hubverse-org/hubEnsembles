#' Example model output data for `simple_ensemble()`
#'
#' Toy model output data formatted according to hubverse standards
#' to be used in the examples for `simple_ensemble()`
#'
#' @format ## `model_outputs`
#' A data frame with 24 rows and 8 columns:
#' \describe{
#'   \item{model_id}{model ID}
#'   \item{location}{FIPS codes}
#'   \item{horizon}{forecast horizon}
#'   \item{target}{forecast target}
#'   \item{target_date}{date that the forecast is for}
#'   \item{output_type}{type of forecast}
#'   \item{output_type_id}{output type ID}
#'   \item{value}{forecast value}
#' }
"model_outputs"

#' Example weights data for `simple_ensemble()`
#'
#' Toy weights data formatted according to hubverse standards
#' to be used in the examples for `simple_ensemble()`
#'
#' @format ## `fweights`
#' A data frame with 8 rows and 3 columns:
#' \describe{
#'   \item{model_id}{model ID}
#'   \item{location}{FIPS codes}
#'   \item{weight}{weight}
#' }
"fweights"


#' Example model output data for `linear_pool()`
#'
#' Toy model output data formatted according to hubverse standards
#' to be used in the examples for `linear_pool()`. The predictions included
#' are taken from three normal distributions with means -3, 0, 3 and
#' all standard deviations 1.
#'
#' @format ## `component_outputs`
#' A data frame with 123 rows and 5 columns:
#' \describe{
#'   \item{model_id}{model ID}
#'   \item{target}{forecast target}
#'   \item{output_type}{type of forecast}
#'   \item{output_type_id}{output type ID}
#'   \item{value}{forecast value}
#' }
"component_outputs"

#' Example weights data for `linear_pool()`
#'
#' Toy weights data formatted according to hubverse standards
#' to be used in the examples for `linear_pool()`. Weights are 0.25, 0.5, 0.25.
#'
#' @format ## `weights`
#' A data frame with 3 rows and 2 columns:
#' \describe{
#'   \item{model_id}{model ID}
#'   \item{location}{FIPS codes}
#'   \item{weight}{weight}
#' }
"weights"
