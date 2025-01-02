create_test_sample_outputs <- function() {
  sample_outputs <- expand.grid(stringsAsFactors = FALSE,
                                model_id = letters[1:4],
                                location = c("222", "888"),
                                horizon = 1, #week
                                target = "inc death",
                                target_date = as.Date("2021-12-25"),
                                output_type = "sample",
                                output_type_id = 1:3,
                                value = NA_real_)

  sample_outputs$value[sample_outputs$location == "222" &
                         sample_outputs$output_type_id == 1] <-
    c(40, 30, 45, 80)
  sample_outputs$value[sample_outputs$location == "222" &
                         sample_outputs$output_type_id == 2] <-
    c(60, 40, 75, 20)
  sample_outputs$value[sample_outputs$location == "222" &
                         sample_outputs$output_type_id == 3] <-
    c(10, 70, 15, 50)
  sample_outputs$value[sample_outputs$location == "888" &
                         sample_outputs$output_type_id == 1] <-
    c(100, 325, 400, 300)
  sample_outputs$value[sample_outputs$location == "888" &
                         sample_outputs$output_type_id == 2] <-
    c(250, 350, 500, 250)
  sample_outputs$value[sample_outputs$location == "888" &
                         sample_outputs$output_type_id == 3] <-
    c(150, 300, 500, 350)

  sample_outputs |>
    dplyr::mutate(horizon = 0, value = 0.75 * .data[["value"]]) |>
    dplyr::bind_rows(sample_outputs)
}
