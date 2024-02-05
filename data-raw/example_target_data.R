## code to prepare `example_target_data` dataset goes here
## note: requires example-complex-forecast-hub and hubEnsembles are
##       cloned into the same folder

hub_path <- "../example-complex-forecast-hub"
target_data_path <- file.path(hub_path, "target-data",
                              "flu-hospitalization-time-series.csv")

example_target_data <- read.csv(target_data_path) |>
    dplyr::mutate(time_idx = as.Date(date))

usethis::use_data(example_target_data, overwrite = TRUE)
