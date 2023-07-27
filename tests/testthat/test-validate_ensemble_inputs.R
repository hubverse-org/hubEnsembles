library(dplyr)

# set up simple data for test cases
model_outputs <- expand.grid(
  stringsAsFactors = FALSE,
  model_id = letters[1:4],
  location = c("222", "888"),
  horizon = 1, #week
  target = "inc death",
  target_date = as.Date("2021-12-25"),
  output_type = "quantile",
  output_type_id = c(.1, .5, .9),
  value = NA_real_)

v2.1 <- model_outputs$value[model_outputs$location == "222" &
                            model_outputs$output_type_id == .1] <-
  c(10, 30, 15, 20)
v2.5 <- model_outputs$value[model_outputs$location == "222" &
                            model_outputs$output_type_id == .5] <-
  c(40, 40, 45, 50)
v2.9 <- model_outputs$value[model_outputs$location == "222" &
                            model_outputs$output_type_id == .9] <-
  c(60, 70, 75, 80)
v8.1 <- model_outputs$value[model_outputs$location == "888" &
                            model_outputs$output_type_id == .1] <-
  c(100, 300, 400, 250)
v8.5 <- model_outputs$value[model_outputs$location == "888" &
                            model_outputs$output_type_id == .5] <-
  c(150, 325, 500, 300)
v8.9 <- model_outputs$value[model_outputs$location == "888" &
                            model_outputs$output_type_id == .9] <-
  c(250, 350, 500, 350)

fweight2 <- data.frame(model_id = letters[1:4],
                       location = "222",
                       weight = 0.1 * (1:4))
fweight8 <- data.frame(model_id = letters[1:4],
                       location = "888",
                       weight = 0.1 * (4:1))
fweight <- bind_rows(fweight2, fweight8)


test_that("invalid output type throws error", {
  expect_error(
    model_outputs %>%
      dplyr::mutate(output_type = "sample") %>%
      validate_ensemble_inputs(valid_output_types=c("quantile"))
  )
})

test_that("weights column already in model_outputs generates error", {
  expect_error(
    model_outputs %>%
      dplyr::mutate(weight = "a") %>%
      validate_ensemble_inputs(weights=fweight, valid_output_types=c("quantile"))
  )
})
