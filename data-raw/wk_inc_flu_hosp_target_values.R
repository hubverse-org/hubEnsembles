## code to prepare `wk-inc-flu-hosp-target-values` dataset goes here
## note: requires example-complex-forecast-hub and hubEnsembles are
##       cloned into the same folder

hub_path <- "../example-complex-forecast-hub"
target_data_path <- file.path(hub_path, "target-data",
                              "wk-inc-flu-hosp-target-values.csv")

wk_inc_flu_hosp_target_values <- read.csv(target_data_path) |>
    dplyr::mutate(reference_date = as.Date(reference_date),
                  target_end_date = as.Date(reference_date))

usethis::use_data(wk_inc_flu_hosp_target_values, overwrite = TRUE)
