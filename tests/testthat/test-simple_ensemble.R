library(hubUtils)
library(matrixStats)
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


test_that("non-default columns are dropped from output", {
  output_names <- model_outputs %>%
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") %>%
    simple_ensemble(
      task_id_cols = c("target_date", "target", "horizon", "location")
    ) %>%
    names()

  expect_equal(sort(names(model_outputs)), sort(output_names))
})


test_that("invalid method argument throws error", {
  expect_error(
    simple_ensemble(model_outputs, agg_fun = "linear pool")
  )
})


test_that("component model outputs and resulting ensemble model outputs have identical sorted unique output type ids", {
  quantile_outputs <- expand.grid(stringsAsFactors = FALSE,
                                  model_id = letters[1:4],
                                  location = c("222", "888"),
                                  horizon = 1, #week
                                  target = "inc death",
                                  target_date = as.Date("2021-12-25"),
                                  output_type = "quantile",
                                  output_type_id = c(.1, .5, .9),
                                  value = NA_real_)

  quantile_outputs$value[quantile_outputs$location == "222" &
                           quantile_outputs$output_type_id == .1] <-
    c(10, 30, 15, 20)
  quantile_outputs$value[quantile_outputs$location == "222" &
                           quantile_outputs$output_type_id == .5] <-
    c(40, 40, 45, 50)
  quantile_outputs$value[quantile_outputs$location == "222" &
                           quantile_outputs$output_type_id == .9] <-
    c(60, 70, 75, 80)
  quantile_outputs$value[quantile_outputs$location == "888" &
                           quantile_outputs$output_type_id == .1] <-
    c(100, 300, 400, 250)
  quantile_outputs$value[quantile_outputs$location == "888" &
                           quantile_outputs$output_type_id == .5] <-
    c(150, 325, 500, 300)
  quantile_outputs$value[quantile_outputs$location == "888" &
                           quantile_outputs$output_type_id == .9] <-
    c(250, 350, 500, 350)

  expected_output_type_ids <- sort(unique(quantile_outputs$output_type_id))

  actual_output_type_ids <- quantile_outputs |>
    simple_ensemble(weights = NULL,
                    weights_col_name = NULL,
                    model_id = "hub-ensemble",
                    task_id_cols = NULL) |>
    dplyr::pull("output_type_id") |>
    unique() |>
    sort()

  expect_equal(expected_output_type_ids, actual_output_type_ids)
})

test_that("group_by(output_type_id) produces expected results", {
  explicit_outputs <- expand.grid(stringsAsFactors = FALSE,
                                  model_id = letters[1:4],
                                  location = c("222", "888"),
                                  horizon = 1, #week
                                  target = "inc death",
                                  target_date = as.Date("2021-12-25"),
                                  output_type = "quantile",
                                  output_type_id = c(.025, .1, .25, .75, .9, .975),
                                  value = NA_real_)

  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .025] <-
    c(4, 12, 6, 8)
  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .100] <-
    c(10, 30, 15, 20)
  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .250] <-
    c(20, 40, 25, 30)
  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .750] <-
    c(50, 50, 55, 60)
  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .900] <-
    c(60, 70, 75, 80)
  explicit_outputs$value[explicit_outputs$location == "222" &
                           explicit_outputs$output_type_id == .975] <-
    c(70, 80, 85, 90)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .025] <-
    c(40, 120, 160, 100)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .100] <-
    c(100, 300, 400, 250)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .250] <-
    c(150, 325, 475, 300)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .750] <-
    c(200, 325, 500, 325)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .900] <-
    c(250, 350, 500, 350)
  explicit_outputs$value[explicit_outputs$location == "888" &
                           explicit_outputs$output_type_id == .975] <-
    c(350, 450, 550, 450)

  intervals <- c(.50, .80, .95)
  implicit_outputs <- explicit_outputs
  implicit_outputs$output_type_id <- sort(rep(c((1 - intervals) / 2, 1 - (1 - intervals) / 2), 8))

  explicit_ensemble <- simple_ensemble(explicit_outputs, weights = NULL,
                                       weights_col_name = NULL,
                                       model_id = "hub-ensemble",
                                       task_id_cols = NULL)

  implicit_ensemble <- simple_ensemble(implicit_outputs, weights = NULL,
                                       weights_col_name = NULL,
                                       model_id = "hub-ensemble",
                                       task_id_cols = NULL)

  expect_equal(explicit_ensemble, implicit_ensemble)
})


test_that("(weighted) medians and means correctly calculated", {
  median_expected <- mean_expected <-
    weighted_median_expected <- weighted_mean_expected <-
    data.frame(model_id = "hub-ensemble",
               location = rep(c("222", "888"), each = 3),
               horizon = 1,
               target = "inc death",
               target_date = as.Date("2021-12-25"),
               output_type = "quantile",
               output_type_id = rep(c(.1, .5, .9), 2),
               value = NA_real_)

  median_vals <- sapply(list(v2_1, v2_5, v2_9, v8_1, v8_5, v8_9), median)
  mean_vals <- sapply(list(v2_1, v2_5, v2_9, v8_1, v8_5, v8_9), mean)

  weighted_median_vals <- c(sapply(list(v2_1, v2_5, v2_9),
                                   matrixStats::weightedMedian,
                                   w = fweight2$weight),
                            sapply(list(v8_1, v8_5, v8_9),
                                   matrixStats::weightedMedian,
                                   w = fweight8$weight))
  weighted_mean_vals <- c(sapply(list(v2_1, v2_5, v2_9),
                                 matrixStats::weightedMean,
                                 w = fweight2$weight),
                          sapply(list(v8_1, v8_5, v8_9),
                                 matrixStats::weightedMean,
                                 w = fweight8$weight))

  median_expected$value <- median_vals
  mean_expected$value <- mean_vals
  weighted_mean_expected$value <- weighted_mean_vals
  weighted_median_expected$value <- weighted_median_vals

  median_actual <- simple_ensemble(model_outputs = model_outputs,
                                   weights = NULL,
                                   agg_fun = "median")
  mean_actual <- simple_ensemble(model_outputs = model_outputs,
                                 weights = NULL,
                                 agg_fun = "mean")

  weighted_median_actual <- simple_ensemble(model_outputs = model_outputs,
                                            weights = fweight,
                                            agg_fun = "median")
  weighted_mean_actual <- simple_ensemble(model_outputs = model_outputs,
                                          weights = fweight,
                                          agg_fun = "mean")

  expect_equal(as.data.frame(median_actual), median_expected)
  expect_equal(as.data.frame(mean_actual), mean_expected)

  expect_equal(as.data.frame(weighted_median_actual), weighted_median_expected)
  expect_equal(as.data.frame(weighted_mean_actual), weighted_mean_expected)
})


test_that("(weighted) medians and means work with alternate name for weights columns", {
  weighted_median_actual <- model_outputs |>
    simple_ensemble(weights = fweight %>% dplyr::rename(w = "weight"),
                    weights_col_name = "w",
                    agg_fun = "median")
  weighted_mean_actual <- model_outputs |>
    simple_ensemble(weights = fweight %>% dplyr::rename(w = "weight"),
                    weights_col_name = "w",
                    agg_fun = "mean")

  weighted_median_expected <- simple_ensemble(model_outputs = model_outputs,
                                              weights = fweight,
                                              agg_fun = "median")
  weighted_mean_expected <- simple_ensemble(model_outputs = model_outputs,
                                            weights = fweight,
                                            agg_fun = "mean")

  expect_equal(weighted_mean_actual, weighted_mean_expected)
  expect_equal(weighted_median_actual, weighted_median_expected)
})


test_that("duplicate forecast values still result in correct weighted median", {
  toy_outputs <- data.frame(
    model_id = letters[1:6],
    output_type = rep("quantile", 6),
    output_type_id = rep(0.5, 6),
    value = c(-0.103, -0.89, 0, 0, 0.039, 0.055)
  )
  weights <- data.frame(
    model_id = letters[1:6],
    weight = c(0.08, 0.14, 0.22, 0.12, 0.28, 0.16)
  )

  weighted_median_expected <- data.frame(
    model_id = "hub-ensemble",
    output_type = "quantile",
    output_type_id = 0.5,
    value = 0
  ) |>
    hubUtils::as_model_out_tbl()

  weighted_median_actual <- toy_outputs |>
    simple_ensemble(weights, agg_fun = "median")

  expect_equal(weighted_median_expected, weighted_median_actual)
})
