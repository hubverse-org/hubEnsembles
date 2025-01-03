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
    res <- linear_pool(
      forecast,
      model_id = "linear-pool-normal",
      compound_taskid_set = c("reference_date", "location", "target_end_date", "target")
    )
  })
  expect_lt(nrow(res), nrow(forecast))
  expect_equal(unique(res$model_id), "linear-pool-normal")

  # Reversing the input gives the same results
  expect_no_error({
    tsacerof <- rev(seq_len(nrow(forecast)))
    ser <- linear_pool(
      forecast[tsacerof, ],
      model_id = "linear-pool-normal",
      compound_taskid_set = c("reference_date", "location", "target_end_date", "target")
    )
  })
  expect_equal(res[res$output_type == "cdf", -1], ser[ser$output_type == "cdf", -1], tolerance = 1e-10)
})


test_that("non-default columns are dropped from output", {
  quantile_outputs <- create_test_quantile_outputs()
  output_names <- quantile_outputs |>
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") |>
    linear_pool(
      task_id_cols = c("target_date", "target", "horizon", "location")
    ) |>
    names()

  expect_equal(sort(names(quantile_outputs)), sort(output_names))
})


test_that("component model outputs and resulting ensemble model outputs have identical sorted unique output type ids", {
  quantile_outputs <- create_test_quantile_outputs()
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


test_that("Not all component models forecasting for the same set of dependent task values throws an error", {
  # There are four models, "a", "b", "c", and "d".
  # The first three have samples for horizons 0 and 1, while model "d" has samples for only horizon 1.
  # The compound task id set doesn't include horizon, so the samples are trajectories over time.
  # We expect to see an error in this case, as we can't combine predictions from models with different
  # subsets of values for variables outside of the compound_taskid_set.
  sample_outputs <- create_test_sample_outputs() |>
    dplyr::filter(model_id %in% letters[1:3] | horizon == 1)

  sample_outputs |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = NULL
    ) |>
    expect_error(
      regex = "Not all component models in `model_out_tbl` forecast for the same set of dependent tasks",
      fixed = TRUE
    )

  # test that df of missing combos returns the expected value
  missing_expected <- data.frame(model_id = letters[4], horizon = 0)
  missing_actual <- sample_outputs |>
    validate_compound_taskid_set(
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      return_missing_combos = TRUE
    )
  expect_equal(missing_actual, dplyr::tibble(missing_expected))
})


test_that("If the specified `compound_taskid_set` is incompatible with component model outputs, throw an error", {
  sample_outputs <- create_test_sample_outputs() |>
    dplyr::mutate(output_type_id = paste0(.data[["location"]], .data[["output_type_id"]]))

  linear_pool(
    sample_outputs,
    weights = NULL,
    task_id_cols = c("target_date", "target", "horizon", "location"),
    compound_taskid_set = c("target", "target_date"),
    n_output_samples = NULL
  ) |>
    expect_error(
      regex = "The specified `compound_taskid_set` is incompatible with ",
      fixed = TRUE
    )
})


test_that(
  "Unequal numbers of samples across component models for unique combination of 
  compound task ID set vars throws an error",
  {
    # there are four models, "a", "b", "c", and "d".
    # The first three models each submit 3 samples, while model "d" submits only 1 sample.
    # We expect an error in this situation, because our methods currently do not support it.
    sample_outputs <- create_test_sample_outputs() |>
      dplyr::filter(model_id %in% letters[1:3] | (output_type_id == 1 & location == "222"))

    linear_pool(
      sample_outputs,
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = NULL
    ) |>
      expect_error(
        regex = "Within each group defined by a combination of the compound task ID set variables",
        fixed = TRUE
      )
  }
)


test_that("Component models can have different sample indexing schemes and be pooled correctly", {
  sample_outputs <- create_test_sample_outputs() |>
    dplyr::mutate(output_type_id = paste0(.data[["model_id"]], .data[["output_type_id"]]))

  expected_outputs <- sample_outputs |>
    dplyr::mutate(
      output_type_id = paste0(.data[["model_id"]], .data[["output_type_id"]]),
      model_id = "hub-ensemble"
    ) |>
    hubUtils::as_model_out_tbl()
  actual_outputs <- sample_outputs |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = NULL
    )
  expect_equal(actual_outputs, expected_outputs)
})


test_that("samples only collected and re-indexed for simplest case", {
  # equal weights, same number of components samples, no limit on output samples
  sample_outputs <- create_test_sample_outputs()
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
  # There are four models, "a", "b", "c", and "d".
  # The first three have samples for locations "222" and "888", while model "d" has samples for only horizon "222".
  # Since not all models forecast for every unique combination of compound task id set variables,
  # the requested output samples will be split differently across those unique combinations
  # We want to ensure that the correct number of output samples are returned
  # and the component models they originate from are as expected
  sample_outputs <- create_test_sample_outputs() |>
    dplyr::filter(model_id %in% letters[1:3] | location == "222", horizon == 1)

  # Summarize outputs by compound task id set and component model to calculate the number of samples
  # that originate from each model per unique combination of compound task id set variables
  lp_outputs_summarized <- sample_outputs |>
    dplyr::mutate(component_model = model_id, .before = 1) |>
    linear_pool(
      weights = NULL,
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = c("target", "location", "target_date"),
      n_output_samples = 6
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("location", "target", "target_date", "component_model")))) |>
    dplyr::summarize(num_forecasts_per_model = dplyr::n())

  # location 222: check all models represented once or twice, total 6 output samples
  num_model_forecasts_222 <- lp_outputs_summarized$num_forecasts_per_model[lp_outputs_summarized$location == "222"]
  expect_in(num_model_forecasts_222, c(1, 2))
  expect_equal(sum(num_model_forecasts_222), 6)

  # location 888: check all models represented twice, total 6 output samples
  num_model_forecasts_888 <- lp_outputs_summarized$num_forecasts_per_model[lp_outputs_summarized$location == "888"]
  expect_in(num_model_forecasts_888, 2)
  expect_equal(sum(num_model_forecasts_888), 6)
})


test_that("ensemble of samples correctly drawn for compound task ID sets", {
  # There are four models, "a", "b", "c", and "d".
  # We want to ensure the samples are pooled correctly for different cases of compound task ids set make up,, e.g.:
  #   (1) The compound task id set consists of a subset of the task id variables
  #   (2) The compound task id set matches all the task id variables exactly
  #   (3) The compound task id set is NULL
  sample_outputs <- create_test_sample_outputs()

  # All compound units have unique output type ids which are shared across
  # non-compound task ids set columns and the expected model is re-sampled
  subset_summarized <- sample_outputs |>
    dplyr::mutate(component_model = model_id, .before = 1) |>
    linear_pool(
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "target", "target_date"),
      n_output_samples = 6
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("location", "target", "target_date", "output_type_id", "component_model")
    ))) |>
    dplyr::summarize(num_forecasts = dplyr::n()) |>
    dplyr::ungroup()

  # per location: check every joint sample forecast is represented 2 or 4 times
  # (once or twice per horizon), total 12 output samples (6 per horizon)
  subset_summarized_222 <- subset_summarized[subset_summarized$location == "222", ]
  expect_in(subset_summarized_222$num_forecasts, 2)
  expect_equal(length(unique(subset_summarized_222$output_type_id)), 6)
  expect_equal(sum(subset_summarized_222$num_forecasts), 12)

  subset_summarized_888 <- subset_summarized[subset_summarized$location == "888", ]
  expect_in(subset_summarized_888$num_forecasts, 2)
  expect_equal(length(unique(subset_summarized_888$output_type_id)), 6)
  expect_equal(sum(subset_summarized_888$num_forecasts), 12)


  # All compound units have unique output type ids and the expected model is re-sampled
  all_tasks_summarized <- sample_outputs |>
    dplyr::mutate(component_model = model_id, .before = 1) |>
    linear_pool(
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "horizon", "target", "target_date"),
      n_output_samples = 6
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("location", "horizon", "target", "target_date", "output_type_id", "component_model")
    ))) |>
    dplyr::summarize(num_forecasts = dplyr::n()) |>
    dplyr::ungroup()

  # per location: check every joint sample forecast is represented once or twice,
  # with between 6 and 12 unique joint sample forecasts, total 12 output samples
  all_tasks_summarized_222 <- all_tasks_summarized[all_tasks_summarized$location == "222", ]
  expect_in(all_tasks_summarized_222$num_forecasts, c(1, 2))
  expect_in(length(unique(all_tasks_summarized_222$output_type_id)), 6:12)
  expect_equal(sum(all_tasks_summarized_222$num_forecasts), 12)

  all_tasks_summarized_888 <- all_tasks_summarized[all_tasks_summarized$location == "888", ]
  expect_in(all_tasks_summarized_888$num_forecasts, c(1, 2))
  expect_in(length(unique(all_tasks_summarized_888$output_type_id)), 6:12)
  expect_equal(sum(all_tasks_summarized_888$num_forecasts), 12)


  # NULL compound task id set variables; unique output type ids are shared across
  # non-compound task ids set columns and the expected model is re-sampled
  none_summarized <- sample_outputs |>
    dplyr::mutate(component_model = model_id, .before = 1) |>
    linear_pool_sample(
      task_id_cols = c("target_date", "target", "horizon", "location"),
      compound_taskid_set = NULL,
      n_output_samples = 6
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("output_type_id", "component_model")))) |>
    dplyr::summarize(num_forecasts = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(-"output_type_id")

  # check every joint sample forecast is represented 4 times, total 24 output samples (6 per compound task id set combo)
  expect_in(none_summarized$num_forecasts, 4)
  expect_equal(sum(none_summarized$num_forecasts), 24)
})

test_that("ensemble of samples throws an error for the more complex cases", {
  sample_outputs <- create_test_sample_outputs()
  fweight <- data.frame(model_id = letters[1:4], weight = 0.1 * (1:4))

  sample_outputs |>
    linear_pool(
      weights = fweight,
      task_id_cols = c("location", "horizon", "target", "target_date"),
      compound_taskid_set = c("location", "target", "target_date"),
      n_output_samples = 20
    ) |>
    dplyr::arrange(output_type_id) |>
    expect_error("`weights` must be \"NULL\" or equal for every model", fixed = TRUE)
})
