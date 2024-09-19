test_that("(#128) linear pool will group by output_type", {
  forecast <- hubExamples::forecast_outputs
  forecast <- forecast[!forecast$output_type %in% c("median", "sample"), ]
  expect_no_error({
    hubEnsembles::linear_pool(forecast, model_id = "linear-pool-normal")
  })
})


test_that("non-default columns are dropped from output", {
  # set up simple data for test cases
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

  cdf_outputs <- dplyr::mutate(quantile_outputs, output_type = "cdf")

  output_names <- quantile_outputs |>
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") |>
    linear_pool(
      task_id_cols = c("target_date", "target", "horizon", "location")
    ) |>
    names()

  expect_equal(sort(names(quantile_outputs)), sort(output_names))
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

  expected_output_type_ids <- data.frame(quantile_outputs) |>
    dplyr::pull("output_type_id") |>
    unique() |>
    sort()

  actual_output_type_ids <- quantile_outputs |>
    linear_pool(weights = NULL,
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

  explicit_ensemble <- linear_pool(explicit_outputs, weights = NULL,
                                   weights_col_name = NULL,
                                   model_id = "hub-ensemble",
                                   task_id_cols = NULL)

  implicit_ensemble <- linear_pool(implicit_outputs, weights = NULL,
                                   weights_col_name = NULL,
                                   model_id = "hub-ensemble",
                                   task_id_cols = NULL)

  expect_equal(explicit_ensemble, implicit_ensemble)
})



test_that("The results are equivalent to those calculated by simple_ensemble for mean, cdf, and pmf output types", {
  mean_outputs <- data.frame(stringsAsFactors = FALSE,
                             model_id = letters[1:3],
                             location = "111",
                             horizon = 1,
                             target = "inc death",
                             target_date = as.Date("2021-12-25"),
                             output_type = "mean",
                             output_type_id = NA,
                             value = c(1, 3, 5))

  fweight1 <- data.frame(model_id = letters[1:3],
                         location = "111",
                         weight = c(0.25, 0.5, 0.25))

  mean_expected <- simple_ensemble(mean_outputs, weights = NULL,
                                   weights_col_name = NULL,
                                   agg_fun = mean,
                                   model_id = "hub-ensemble",
                                   task_id_cols = NULL)

  mean_actual <- linear_pool(mean_outputs, weights = NULL,
                             weights_col_name = NULL,
                             model_id = "hub-ensemble",
                             task_id_cols = NULL)

  weighted_mean_expected <- simple_ensemble(mean_outputs, weights = fweight1,
                                            weights_col_name = "weight",
                                            agg_fun = matrixStats::weightedMean,
                                            model_id = "hub-ensemble",
                                            task_id_cols = NULL)

  weighted_mean_actual <- linear_pool(mean_outputs, weights = fweight1,
                                      weights_col_name = "weight",
                                      model_id = "hub-ensemble",
                                      task_id_cols = NULL)

  expect_equal(mean_expected, mean_actual, tolerance = 1e-3)
  expect_equal(weighted_mean_expected, weighted_mean_actual, tolerance = 1e-3)
})


test_that("(weighted) quantiles correctly calculated", {
  # The three component models provide quantiles from the distributions
  # F_1 = N(-3, 1), F_2 = N(0,1), and F_3 = N(3, 1)
  # The linear pool is a (weighted) mixture with cdf F(x) = \sum_m w_m F_m(x)
  # We test with equal weights w_m = 1/3 and with weights w_1 = 0.25, w_2 = 0.5, w_3 = 0.25
  quantile_expected <-
    weighted_quantile_expected <-
    data.frame(stringsAsFactors = FALSE,
               model_id = "hub-ensemble",
               location = "111",
               horizon = 1,
               target = "inc death",
               target_date = as.Date("2021-12-25"),
               output_type = "quantile",
               output_type_id = rep(NA, 21),
               value = NA_real_)

  quantile_values <- weighted_quantile_values <- seq(from = -5, to = 5, by = 0.5) # expected
  output_prob <- stats::pnorm(quantile_values, mean = -3) / 3 +
    stats::pnorm(quantile_values, mean = 0) / 3 +
    stats::pnorm(quantile_values, mean = 3) / 3
  weighted_output_prob <- 0.25 * stats::pnorm(quantile_values, mean = -3) +
    0.5 * stats::pnorm(quantile_values, mean = 0) +
    0.25 * stats::pnorm(quantile_values, mean = 3)

  quantile_expected$value <- weighted_quantile_expected$value <- quantile_values
  quantile_expected$output_type_id <- output_prob
  weighted_quantile_expected$output_type_id <- weighted_output_prob

  component_outputs <- expand.grid(stringsAsFactors = FALSE,
                                   model_id = letters[1:3],
                                   location = "111",
                                   horizon = 1,
                                   target = "inc death",
                                   target_date = as.Date("2021-12-25"),
                                   output_type = "quantile",
                                   output_type_id = output_prob,
                                   value = NA_real_)

  component_outputs$value[component_outputs$model_id == "a"] <-
    stats::qnorm(output_prob, mean = -3)
  component_outputs$value[component_outputs$model_id == "b"] <-
    stats::qnorm(output_prob, mean = 0)
  component_outputs$value[component_outputs$model_id == "c"] <-
    stats::qnorm(output_prob, mean = 3)

  weighted_component_outputs <- expand.grid(stringsAsFactors = FALSE,
                                            model_id = letters[1:3],
                                            location = "111",
                                            horizon = 1,
                                            target = "inc death",
                                            target_date = as.Date("2021-12-25"),
                                            output_type = "quantile",
                                            output_type_id = weighted_output_prob,
                                            value = NA_real_)

  weighted_component_outputs$value[weighted_component_outputs$model_id == "a"] <-
    stats::qnorm(weighted_output_prob, mean = -3)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "b"] <-
    stats::qnorm(weighted_output_prob, mean = 0)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "c"] <-
    stats::qnorm(weighted_output_prob, mean = 3)

  fweight1 <- data.frame(model_id = letters[1:3],
                         location = "111",
                         weight = c(0.25, 0.5, 0.25))

  quantile_actual <- linear_pool(component_outputs, weights = NULL,
                                 weights_col_name = NULL,
                                 model_id = "hub-ensemble",
                                 task_id_cols = NULL)

  weighted_quantile_actual <- linear_pool(weighted_component_outputs,
                                          weights = fweight1,
                                          weights_col_name = "weight",
                                          model_id = "hub-ensemble",
                                          task_id_cols = NULL)

  expect_equal(quantile_expected,
               as.data.frame(quantile_actual),
               tolerance = 1e-3)
  expect_equal(weighted_quantile_expected,
               as.data.frame(weighted_quantile_actual),
               tolerance = 1e-2)
})



test_that("(weighted) quantiles correctly calculated - lognormal family", {
  # The three component models provide quantiles from the distributions
  # F_1 = lognorm(-3, 1), F_2 = lognorm(0,1), and F_3 = lognorm(3, 1)
  # The linear pool is a (weighted) mixture with cdf F(x) = \sum_m w_m F_m(x)
  # We test with equal weights w_m = 1/3 and with weights w_1 = 0.25, w_2 = 0.5, w_3 = 0.25
  quantile_values <- weighted_quantile_values <- exp(seq(from = -3, to = 3, by = 0.5)) # expected

  quantile_expected <-
    weighted_quantile_expected <-
    data.frame(stringsAsFactors = FALSE,
               model_id = "hub-ensemble",
               location = "111",
               horizon = 1,
               target = "inc death",
               target_date = as.Date("2021-12-25"),
               output_type = "quantile",
               output_type_id = rep(NA, length(quantile_values)),
               value = NA_real_)

  output_prob <- stats::plnorm(quantile_values, mean = -3) / 3 +
    stats::plnorm(quantile_values, mean = 0) / 3 +
    stats::plnorm(quantile_values, mean = 3) / 3
  weighted_output_prob <- 0.25 * stats::plnorm(quantile_values, mean = -3) +
    0.5 * stats::plnorm(quantile_values, mean = 0) +
    0.25 * stats::plnorm(quantile_values, mean = 3)

  quantile_expected$value <- weighted_quantile_expected$value <- quantile_values
  quantile_expected$output_type_id <- output_prob
  weighted_quantile_expected$output_type_id <- weighted_output_prob

  component_outputs <- expand.grid(stringsAsFactors = FALSE,
                                   model_id = letters[1:3],
                                   location = "111",
                                   horizon = 1,
                                   target = "inc death",
                                   target_date = as.Date("2021-12-25"),
                                   output_type = "quantile",
                                   output_type_id = output_prob,
                                   value = NA_real_)

  component_outputs$value[component_outputs$model_id == "a"] <-
    stats::qlnorm(output_prob, mean = -3)
  component_outputs$value[component_outputs$model_id == "b"] <-
    stats::qlnorm(output_prob, mean = 0)
  component_outputs$value[component_outputs$model_id == "c"] <-
    stats::qlnorm(output_prob, mean = 3)

  weighted_component_outputs <- expand.grid(stringsAsFactors = FALSE,
                                            model_id = letters[1:3],
                                            location = "111",
                                            horizon = 1,
                                            target = "inc death",
                                            target_date = as.Date("2021-12-25"),
                                            output_type = "quantile",
                                            output_type_id = weighted_output_prob,
                                            value = NA_real_)

  weighted_component_outputs$value[weighted_component_outputs$model_id == "a"] <-
    stats::qlnorm(weighted_output_prob, mean = -3)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "b"] <-
    stats::qlnorm(weighted_output_prob, mean = 0)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "c"] <-
    stats::qlnorm(weighted_output_prob, mean = 3)

  fweight1 <- data.frame(model_id = letters[1:3],
                         location = "111",
                         weight = c(0.25, 0.5, 0.25))

  quantile_actual_norm <- linear_pool(component_outputs, weights = NULL,
                                      weights_col_name = NULL,
                                      model_id = "hub-ensemble",
                                      task_id_cols = NULL,
                                      n_samples = 1e5)

  weighted_quantile_actual_norm <- linear_pool(weighted_component_outputs,
                                               weights = fweight1,
                                               weights_col_name = "weight",
                                               model_id = "hub-ensemble",
                                               task_id_cols = NULL,
                                               n_samples = 1e5)

  quantile_actual_lnorm <- linear_pool(component_outputs, weights = NULL,
                                       weights_col_name = NULL,
                                       model_id = "hub-ensemble",
                                       task_id_cols = NULL,
                                       tail_dist = "lnorm",
                                       n_samples = 1e5)

  weighted_quantile_actual_lnorm <- linear_pool(weighted_component_outputs,
                                                weights = fweight1,
                                                weights_col_name = "weight",
                                                model_id = "hub-ensemble",
                                                task_id_cols = NULL,
                                                tail_dist = "lnorm",
                                                n_samples = 1e5)

  expect_false(isTRUE(all.equal(quantile_expected, as.data.frame(quantile_actual_norm),
                                tolerance = 1e-3)))
  expect_false(isTRUE(all.equal(weighted_quantile_expected, as.data.frame(weighted_quantile_actual_norm),
                                tolerance = 1e-2)))

  expect_equal(quantile_expected, as.data.frame(quantile_actual_lnorm),
               tolerance = 1e-3)
  expect_equal(weighted_quantile_expected, as.data.frame(weighted_quantile_actual_lnorm),
               tolerance = 1e-2)
})
