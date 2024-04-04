## code to prepare `example_model_output` dataset goes here
## note: requires example-complex-forecast-hub and hubEnsembles are
##       cloned into the same folder

library(hubData)
hub_path <- "../example-complex-forecast-hub"
example_model_output <- hubData::connect_hub(hub_path) |>
  dplyr::collect() |>
  dplyr::select(model_id, location, reference_date, horizon, target_end_date,
                target, output_type, output_type_id, value)

usethis::use_data(example_model_output, overwrite = TRUE)
