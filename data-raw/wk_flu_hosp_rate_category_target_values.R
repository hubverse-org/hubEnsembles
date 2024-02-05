## code to prepare `wk_flu_hosp_rate_category_target_values` dataset goes here
## note: requires example-complex-forecast-hub and hubEnsembles are
##       cloned into the same folder

hub_path <- "../example-complex-forecast-hub"
target_data_path <- file.path(hub_path, "target-data",
                              "wk-flu-hosp-rate-category-target-values.csv")

wk_flu_hosp_rate_category_target_values <- read.csv(target_data_path) |>
    dplyr::mutate(reference_date = as.Date(reference_date),
                  target_end_date = as.Date(reference_date))

usethis::use_data(wk_flu_hosp_rate_category_target_values, overwrite = TRUE)

