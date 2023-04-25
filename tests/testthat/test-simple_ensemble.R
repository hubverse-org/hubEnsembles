library(matrixStats)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#tmp_dat <- readr::read_csv("test-data/minimal-forecast.csv")

baseline_1001 <- readr::read_csv("inst/test-data/2022-10-01-simple_hub-baseline.csv") %>%
  mutate(team_abbr = "simple_hub", model_abbr = "baseline", .before = origin_date)
baseline_1008 <- readr::read_csv("inst/test-data/2022-10-08-simple_hub-baseline.csv") %>%
  mutate(team_abbr = "simple_hub", model_abbr = "baseline", .before = origin_date)
team1_1008 <- readr::read_csv("inst/test-data/2022-10-08-team1-goodmodel.csv") %>%
  mutate(team_abbr = "team_1", model_abbr = "goodmodel1", .before = origin_date) %>%
  mutate(value = value + 2)

test_that("invalid method argument throws error", {
  expect_error(
    simple_ensemble(
      baseline_1008,
      agg_fun="linear pool",
      model_abbr = "example"
    )
  )
})

test_that("medians and means correctly calculated", {
  fdat <- expand.grid(
    stringsAsFactors = FALSE,
    model_abbr = letters[1:4],
    location = c("222", "888"),
    horizon = 1, #week
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    output_type = "quantile",
    output_id = c(.1, .5, .9))

  fdat <- cbind(team_abbr = letters[23:26], fdat)
  fdat$value[fdat$location == "222" & fdat$output_id == .1] <- v2.1 <- c(10, 30, 15, 20)
  fdat$value[fdat$location == "222" & fdat$output_id == .5] <- v2.5 <- c(40, 40, 45, 50)
  fdat$value[fdat$location == "222" & fdat$output_id == .9] <- v2.9 <- c(60, 70, 75, 80)
  fdat$value[fdat$location == "888" & fdat$output_id == .1] <- v8.1 <- c(100, 300, 400, 250)
  fdat$value[fdat$location == "888" & fdat$output_id == .5] <- v8.5 <- c(150, 325, 500, 300)
  fdat$value[fdat$location == "888" & fdat$output_id == .9] <- v8.9 <- c(250, 350, 500, 350)

  median_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), median)
  mean_vals <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), mean)

  fweight <- tibble(model_abbr = letters[1:4], weight = 0.1*(1:4))

  weighted_median_vals <- map(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), weightedMedian, w = fweight$weight)
  weighted_mean_vals <- map(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), weightedMean, w = fweight$weight)

  median_actual <- simple_ensemble(
    predictions = fdat, weights = NULL, agg_fun = "median", model_abbr = "median_ens")
  mean_actual <- simple_ensemble(
    predictions = fdat, weights = NULL, agg_fun = "mean", model_abbr = "mean_ens")
    
  weighted_median_actual <- simple_ensemble(
    predictions = fdat, weights = fweight, agg_fun = "median", model_abbr = "weighted_median_ens")
  weighted_mean_actual <- simple_ensemble(
    predictions = fdat, weights = fweight, agg_fun = "mean", model_abbr = "weighted_mean_ens")


  median_expected <- mean_expected <- weighted_median_expected <- weighted_mean_expected <- tibble::tibble(
    team_abbr = "Hub",
    model_abbr = NA,
    location = rep(c("222", "888"), each = 3), 
    horizon = 1, #week
    target = "inc death",     
    target_date = as.Date("2021-12-25"), 
    output_type = "quantile", 
    output_id = rep(c(.1, .5, .9), 2), 
    value = 0)

  median_expected$value <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), median)
  median_expected$model_abbr <- "median_ens"
  mean_expected$value <- sapply(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), mean)
  mean_expected$model_abbr <- "mean_ens"

  weighted_mean_expected$value <- map_dbl(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), weightedMean, w = fweight$weight)
  weighted_mean_expected$model_abbr <- "weighted_mean_ens"
  weighted_median_expected$value <- map_dbl(list(v2.1, v2.5, v2.9, v8.1, v8.5, v8.9), weightedMedian, w = fweight$weight)
  weighted_median_expected$model_abbr <- "weighted_median_ens"

  expect_equal(median_actual, median_expected)
  expect_equal(mean_actual, mean_expected)
  
  expect_equal(weighted_median_actual, weighted_median_expected)
  expect_equal(weighted_mean_actual, weighted_mean_expected)
})