## code to prepare `example_model_output` dataset goes here
## note: requires example-complex-forecast-hub and hubEnsembles are
##       cloned into the same folder

library(hubUtils)
hub_path <- "../example-complex-forecast-hub"
example_model_output <- hubUtils::connect_hub(hub_path) |>
    dplyr::collect()

usethis::use_data(example_model_output, overwrite = TRUE)

