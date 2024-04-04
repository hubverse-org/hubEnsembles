#' Example target data
#'
#' Example target data consisting of incident influenza hospitalization admissions
#' in the US from 2020-2023. This data can be useful as a modeling input and for
#'  plots of time series values.
#'
#' @format ## `example_target_data`
#' A data frame with 10,255 rows and 3 columns:
#' \describe{
#'   \item{time_idx}{the Saturday ending the week during which hospital admission
#'                   were recorded, in format `yyyy-mm-dd`}
#'   \item{location}{FIPS code identifying a location}
#'   \item{value}{the number of influenza hospital admissions in the specified
#'                `location` over the course of the week ending on the
#'                specified date, `time_idx`}
#'   ...
#' }
#' @source <https://github.com/Infectious-Disease-Modeling-Hubs/example-complex-forecast-hub/>
"example_target_data"

#' Example model output data
#'
#' Example model outputs adapted from forecasts submitted to the FluSight
#' Forecast Hub for the 2022/23 season. Model outputs data include `quantile`,
#' `median`, `mean` forecasts of future incident influenza hospitalization
#' admissions, and `pmf` forecasts of hospitalization intensity,
#' including “low”, “moderate”, “high”, or “very high” intensities.
#' Hospitalization intensity categories are determined by thresholds for weekly
#' hospital admissions per 100,000 population. The example model output data
#' should be viewed only as illustrations of the data formats, not as realistic
#' examples of forecasts.
#'
#' @format ## `example_model_output`
#' A data frame with 70,064 rows and 9 columns:
#' \describe{
#'   \item{model_id}{unique identifier for the model submitting forecasts}
#'   \item{location}{FIPS code identifying a location}
#'   \item{reference_date}{the starting point of the forecasts}
#'   \item{horizon}{number of weeks ahead being forecasted}
#'   \item{target_end_date}{the date of occurrence of the outcome of interest;
#'          this can be calculated directly from the `reference_date` and `horizon`
#'          as follows: `target_end_date = reference_date + 7*horizon`}
#'   \item{target}{a unique identifier for the target}
#'   \item{output_type}{the type of representation of the prediction}
#'   \item{output_type_id}{more identifying information specific to the output type}
#'   \item{value}{the model’s prediction}
#' }
#' See [hubverse documentation](https://hubdocs.readthedocs.io/en/latest/user-guide/model-output.html),
#' for more information about model output formats.
#'
#' @source <https://github.com/Infectious-Disease-Modeling-Hubs/example-complex-forecast-hub/>
"example_model_output"
