library(dplyr)

# set up simple data for test cases
model_outputs <- expand.grid(stringsAsFactors = FALSE,
                             model_id = letters[1:4],
                             location = c("222", "888"),
                             horizon = 1, #week
                             target = "inc death",
                             target_date = as.Date("2021-12-25"),
                             output_type = "quantile",
                             output_type_id = c(.1, .5, .9),
                             value = NA_real_)

v2_1 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .1] <-
  c(10, 30, 15, 20)
v2_5 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .5] <-
  c(40, 40, 45, 50)
v2_9 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .9] <-
  c(60, 70, 75, 80)
v8_1 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .1] <-
  c(100, 300, 400, 250)
v8_5 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .5] <-
  c(150, 325, 500, 300)
v8_9 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .9] <-
  c(250, 350, 500, 350)

fweight2 <- data.frame(model_id = letters[1:4],
                       location = "222",
                       weight = 0.1 * (1:4))
fweight8 <- data.frame(model_id = letters[1:4],
                       location = "888",
                       weight = 0.1 * (4:1))
fweight <- bind_rows(fweight2, fweight8)


test_that("mismatched task_id_cols throws error", {
  expect_error(
    model_outputs |>
      validate_ensemble_inputs(task_id_cols = "reference_date")
  )
})

test_that("NULL task_id_cols does not throw an error", {
  expect_error(
    model_outputs |>
      validate_ensemble_inputs(task_id_cols = NULL)
  )
})

test_that("invalid output type throws error", {
  expect_error(
    model_outputs |>
      dplyr::mutate(output_type = "sample") |>
      validate_ensemble_inputs(valid_output_types = c("quantile"))
  )
})

test_that("no error if models provide the same output_type_ids", {
  expect_no_error(
    validate_output_type_ids(
      model_outputs,
      task_id_cols = c("location", "horizon", "target", "target_date")
    )
  )
})

test_that("error if models provide different output_type_ids", {
  expect_error(
    model_outputs |>
      dplyr::filter(!(model_id == "b" & abs(output_type_id - 0.5) < 1e-6)) |>
      validate_output_type_ids(task_id_cols = c("location", "horizon", "target", "target_date"))
  )
})


test_that("weights missing required columns generates error", {
  expect_error(
    model_outputs |>
      validate_ensemble_inputs(weights = fweight |> dplyr::select(weight))
  )
})

test_that("value column in weights generates error", {
  expect_error(
    model_outputs |>
      validate_ensemble_inputs(weights = fweight |> dplyr::mutate(value = 0.25))
  )
})

test_that("column not from model_outputs in weights generates error", {
  expect_error(
    model_outputs |>
      validate_ensemble_inputs(weights = fweight |> dplyr::mutate(age_group = "65+"))
  )
})

test_that("weights column already in model_outputs generates error", {
  expect_error(
    model_outputs |>
      dplyr::mutate(weight = "a") |>
      validate_ensemble_inputs(weights = fweight, valid_output_types = c("quantile"))
  )
})

test_that("error if weights depend on output_type_id for cdf and pmf output_type", {
  fweight <- expand.grid(model_id = letters[1:4],
                         output_type_id = c(0.1, 0.5, 0.9),
                         weight = NA,
                         stringsAsFactors = FALSE)
  expect_error(
    model_outputs |>
      dplyr::mutate(output_type = "pmf", value = 1 / value) |>
      validate_ensemble_inputs(weights = fweight, valid_output_types = c("pmf"))
  )
})

test_that("validated model_output is a model_out_tbl", {
  validated_outputs <- validate_ensemble_inputs(model_outputs, valid_output_types = "quantile")[[1]]
  expect_true(inherits(validated_outputs, "model_out_tbl"))
})
