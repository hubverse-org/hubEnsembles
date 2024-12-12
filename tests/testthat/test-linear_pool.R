test_that("(#128) linear pool will group by output_type", {
  skip_if_not_installed("hubExamples")
  forecast <- hubExamples::forecast_outputs
  expect_warning({
    forecast <- forecast[!forecast$output_type %in% c("median"), ] |>
      dplyr::mutate(output_type_id = ifelse(
        as.numeric(output_type_id) %in% 4201:4300,
        as.character(as.numeric(output_type_id) + 100),
        output_type_id
      ))
  })
  expect_no_error({
    res <- linear_pool(forecast, model_id = "linear-pool-normal")
  })
  expect_lt(nrow(res), nrow(forecast))
  expect_equal(unique(res$model_id), "linear-pool-normal")

  # Reversing the input gives the same results
  expect_no_error({
    tsacerof <- rev(seq_len(nrow(forecast)))
    ser <- linear_pool(forecast[tsacerof, ], model_id = "linear-pool-normal")
  })
  expect_equal(res[res$output_type == "cdf", -1], ser[ser$output_type == "cdf", -1], tolerance = 1e-10)
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
               tolerance = 1e-3)
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
                                tolerance = 1e-3)))

  expect_equal(quantile_expected, as.data.frame(quantile_actual_lnorm),
               tolerance = 1e-3)
  expect_equal(weighted_quantile_expected, as.data.frame(weighted_quantile_actual_lnorm),
               tolerance = 1e-3)
})


test_that("all component models submit the same values for non-compound task id set variables for sample forecasts", {
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
    dplyr::mutate(horizon = 0, value = 0.75 * value) |>
    dplyr::filter(model_id %in% letters[1:3]) |>
    dplyr::bind_rows(sample_outputs) |>
    dplyr::mutate(origin_date = target_date - horizon, .before = "horizon") |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location", "origin_date"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = NULL
    ) |>
    expect_error(
      regex = "Not all component models in `model_out_tbl` forecast for the same set of dependent tasks",
      fixed = TRUE
    )
})


test_that("samples only collected and re-indexed for simplest case", {
  # equal weights, same number of components samples, no limit on output samples
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

  expected_outputs <- sample_outputs |>
    dplyr::mutate(
      output_type_id = as.integer(factor(paste0(.data[["model_id"]], .data[["output_type_id"]]))),
      model_id = "hub-ensemble"
    ) |>
    hubUtils::as_model_out_tbl()
  actual_outputs <- sample_outputs |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location"),
      n_output_samples = NULL
    )

  expect_equal(actual_outputs, expected_outputs)
})


test_that("remainder samples are properly distributed when component models don't all forecast for every location", {
  sample_outputs <- expand.grid(stringsAsFactors = FALSE,
                                model_id = letters[1:4],
                                location = c("222", "888"),
                                horizon = 1, #week
                                target = "inc death",
                                target_date = as.Date("2021-12-25"),
                                output_type = "sample",
                                output_type_id = 1:3,
                                value = NA_real_) |>
    dplyr::filter(model_id %in% letters[1:3] | location == "222")

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
    c(100, 325, 400)
  sample_outputs$value[sample_outputs$location == "888" &
                         sample_outputs$output_type_id == 2] <-
    c(250, 350, 500)
  sample_outputs$value[sample_outputs$location == "888" &
                         sample_outputs$output_type_id == 3] <-
    c(150, 300, 500)

  set.seed(1234)
  models_to_resample <- sample(x = letters[1:4], size = 6 %% 4)
  expected_outputs <- expand.grid(
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE,
    location = c("222", "888"),
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    component_model = letters[1:6],
    distinct_ids = 1
  )
  expected_outputs$component_model[expected_outputs$location == "222" &
                                     expected_outputs$component_model %in% letters[5:6]] <- models_to_resample
  expected_outputs$component_model[expected_outputs$location == "888" &
                                     expected_outputs$component_model %in% letters[4:6]] <- letters[1:3]
  expected_outputs <- expected_outputs |>
    dplyr::arrange(dplyr::across(c("location", "target", "target_date", "component_model"))) |>
    dplyr::tibble()

  set.seed(1234)
  actual_outputs <- sample_outputs |>
    dplyr::mutate(component_model = .data[["model_id"]], .before = 1) |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = 6
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("location", "target", "target_date", "output_type_id", "component_model")
    ))) |>
    dplyr::summarize(distinct_ids = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-"output_type_id")
  expect_equal(actual_outputs, expected_outputs)
})


test_that("ensemble of samples correctly drawn for compound task ID sets", {
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
  sample_outputs <- sample_outputs |>
    dplyr::mutate(horizon = 0) |>
    dplyr::bind_rows(sample_outputs)

  # All compound units have unique ids which are shared across dependent task columns
  # expected model is re-sampled
  set.seed(1234)
  models_to_resample <- sample(x = letters[1:4], size = 5 %% 4)
  subset_expected <- expand.grid(
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE,
    location = c("222", "888"),
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    component_model = letters[1:5],
    distinct_ids = 2
  ) |>
    dplyr::mutate(component_model = ifelse(component_model == letters[5], models_to_resample, component_model)) |>
    dplyr::arrange(dplyr::across(c("location", "target", "target_date"))) |>
    dplyr::tibble()
  set.seed(1234)
  subset_actual <- sample_outputs |>
    dplyr::mutate(component_model = .data[["model_id"]], .before = 1) |>
    linear_pool(
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "target", "target_date"),
      n_output_samples = 5
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("location", "target", "target_date", "output_type_id", "component_model")
    ))) |>
    dplyr::summarize(distinct_ids = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-"output_type_id")
  expect_equal(subset_actual, subset_expected)

  # All compound units have unique ids, expected model is re-sampled
  set.seed(1234)
  models_to_resample <- sample(x = letters[1:4], size = 6 %% 4)
  all_tasks_expected <- expand.grid(
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE,
    location = c("222", "888"),
    horizon = c(0, 1),
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    component_model = letters[1:5],
    distinct_ids = 1
  )
  all_tasks_expected$component_model[all_tasks_expected$component_model == letters[5]
                                     & all_tasks_expected$location == "222"] <- models_to_resample
  all_tasks_expected$component_model[all_tasks_expected$component_model == letters[5]
                                     & all_tasks_expected$location == "888"] <- models_to_resample
  all_tasks_expected <- all_tasks_expected |>
    dplyr::arrange(dplyr::across(dplyr::all_of(
      c("location", "horizon", "target", "target_date", "component_model")
    ))) |>
    dplyr::tibble()
  set.seed(1234)
  all_tasks_actual <- sample_outputs |>
    dplyr::mutate(component_model = .data[["model_id"]], .before = 1) |>
    linear_pool(
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "horizon", "target", "target_date"),
      n_output_samples = 5
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("location", "horizon", "target", "target_date", "output_type_id", "component_model")
    ))) |>
    dplyr::summarize(distinct_ids = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-"output_type_id")
  expect_equal(all_tasks_actual, all_tasks_expected)

  # No compound units; unique ids are shared across dependent task columns
  # expected model is re-sampled
  none_expected <- expand.grid(
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE,
    component_model = letters[1:5],
    distinct_ids = 4
  ) |>
    dplyr::mutate(component_model = ifelse(component_model == letters[5], models_to_resample, component_model)) |>
    dplyr::tibble()
  set.seed(1234)
  none_actual <- sample_outputs |>
    dplyr::mutate(component_model = .data[["model_id"]], .before = 1) |>
    linear_pool_sample(
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = NULL,
      n_output_samples = 5
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("output_type_id", "component_model")))) |>
    dplyr::summarize(distinct_ids = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-"output_type_id")
  expect_equal(none_actual, none_expected)
})

test_that("ensemble of samples throws an error for the more complex cases", {
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

  fweight <- data.frame(model_id = letters[1:4], weight = 0.1 * (1:4))

  sample_outputs |>
    linear_pool(
      weights = fweight,
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "target", "target_date"),
      n_output_samples = 20
    ) |>
    dplyr::arrange(.data[["output_type_id"]]) |>
    expect_error("`weights` must be \"NULL\" or equal for every model", fixed = TRUE)
})
