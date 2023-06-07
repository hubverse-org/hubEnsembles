library(matrixStats)
library(dplyr)

pred <- read.csv(system.file("example-data/2022-10-08-simple_hub-baseline.csv",
                             package = "hubEnsembles")) %>%
  dplyr::mutate(model_id = "simple_hub-baseline", .before = origin_date)

test_that("non-default columns are dropped from output", {
  output_names <- pred %>%
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") %>%
    simple_ensemble(
      task_id_cols = c("origin_date", "target", "horizon", "location")
    ) %>%
    names()

  expect_equal(sort(names(pred)), sort(output_names))
})


test_that("invalid output type throws error", {
  expect_error(
    pred %>%
      dplyr::mutate(output_type = "sample") %>%
      simple_ensemble()
  )
})


test_that("invalid method argument throws error", {
  expect_error(
    simple_ensemble(pred, agg_fun="linear pool")
  )
})


test_that("(weighted) medians and means correctly calculated", {
  fdat <- expand.grid(
    stringsAsFactors = FALSE,
    model_id = letters[1:4],
    location = c("222", "888"),
    horizon = 1, #week
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    output_type = "quantile",
    output_type_id = c(.1, .5, .9),
    value = NA_real_)

  fdat$value[fdat$location == "222" & fdat$output_type_id == .1] <- v2.1 <-
    c(10, 30, 15, 20)
  fdat$value[fdat$location == "222" & fdat$output_type_id == .5] <- v2.5 <-
    c(40, 40, 45, 50)
  fdat$value[fdat$location == "222" & fdat$output_type_id == .9] <- v2.9 <-
    c(60, 70, 75, 80)
  fdat$value[fdat$location == "888" & fdat$output_type_id == .1] <- v8.1 <-
    c(100, 300, 400, 250)
  fdat$value[fdat$location == "888" & fdat$output_type_id == .5] <- v8.5 <-
    c(150, 325, 500, 300)
  fdat$value[fdat$location == "888" & fdat$output_type_id == .9] <- v8.9 <-
    c(250, 350, 500, 350)

  fweight2 <- data.frame(model_id = letters[1:4],
                         location = "222",
                         weight = 0.1 * (1:4))
  fweight8 <- data.frame(model_id = letters[1:4],
                         location = "888",
                         weight = 0.1 * (4:1))
  fweight <- bind_rows(fweight2, fweight8)

  median_expected <- mean_expected <-
    weighted_median_expected <- weighted_mean_expected <- data.frame(
      model_id = "hub-ensemble",
      location = rep(c("222", "888"), each = 3),
      horizon = 1,
      target = "inc death",
      target_date = as.Date("2021-12-25"), 
      output_type = "quantile", 
      output_type_id = rep(c(.1, .5, .9), 2), 
      value = NA_real_)

  median_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), median)
  mean_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), mean)

  weighted_median_vals <- c(
    sapply(list(v2.1, v2.5, v2.9),
           matrixStats::weightedMedian,
           w = fweight2$weight),
    sapply(list(v8.1, v8.5, v8.9),
           matrixStats::weightedMedian,
           w = fweight8$weight))
  weighted_mean_vals <- c(
    sapply(list(v2.1, v2.5, v2.9),
           matrixStats::weightedMean,
           w = fweight2$weight),
    sapply(list(v8.1, v8.5, v8.9),
           matrixStats::weightedMean,
           w = fweight8$weight))

  median_expected$value <- median_vals
  mean_expected$value <- mean_vals
  weighted_mean_expected$value <- weighted_mean_vals
  weighted_median_expected$value <- weighted_median_vals

  median_actual <- simple_ensemble(model_outputs = fdat, weights = NULL,
                                   agg_fun = "median")
  mean_actual <- simple_ensemble(model_outputs = fdat, weights = NULL,
                                 agg_fun = "mean")

  weighted_median_actual <- simple_ensemble(model_outputs = fdat,
                                            weights = fweight,
                                            agg_fun = "median")
  weighted_mean_actual <- simple_ensemble(model_outputs = fdat,
                                          weights = fweight,
                                          agg_fun = "mean")

  expect_equal(as.data.frame(median_actual), median_expected)
  expect_equal(as.data.frame(mean_actual), mean_expected)

  expect_equal(as.data.frame(weighted_median_actual), weighted_median_expected)
  expect_equal(as.data.frame(weighted_mean_actual), weighted_mean_expected)
})
