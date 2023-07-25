#library(Hmisc)
#library(distfromq)
library(matrixStats)
library(dplyr)
library(tidyr)

# set up simple data for test cases
quantile_outputs <- expand.grid(
  stringsAsFactors = FALSE,
  model_id = letters[1:4],
  location = c("222", "888"),
  horizon = 1, #week
  target = "inc death",
  target_date = as.Date("2021-12-25"),
  output_type = "quantile",
  output_type_id = c(.1, .5, .9),
  value = NA_real_)

v2.1 <- quantile_outputs$value[quantile_outputs$location == "222" &
                            quantile_outputs$output_type_id == .1] <-
  c(10, 30, 15, 20)
v2.5 <- quantile_outputs$value[quantile_outputs$location == "222" &
                            quantile_outputs$output_type_id == .5] <-
  c(40, 40, 45, 50)
v2.9 <- quantile_outputs$value[quantile_outputs$location == "222" &
                            quantile_outputs$output_type_id == .9] <-
  c(60, 70, 75, 80)
v8.1 <- quantile_outputs$value[quantile_outputs$location == "888" &
                            quantile_outputs$output_type_id == .1] <-
  c(100, 300, 400, 250)
v8.5 <- quantile_outputs$value[quantile_outputs$location == "888" &
                            quantile_outputs$output_type_id == .5] <-
  c(150, 325, 500, 300)
v8.9 <- quantile_outputs$value[quantile_outputs$location == "888" &
                            quantile_outputs$output_type_id == .9] <-
  c(250, 350, 500, 350)

cdf_outputs <- mutate(quantile_outputs,output_type="cdf")

fweight2 <- data.frame(model_id = letters[1:4],
                       location = "222",
                       weight = 0.1 * (1:4))
fweight8 <- data.frame(model_id = letters[1:4],
                       location = "888",
                       weight = 0.1 * (4:1))
fweight <- bind_rows(fweight2, fweight8)


test_that("non-default columns are dropped from output", {
  output_names <- quantile_outputs %>%
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") %>%
    linear_pool(
      task_id_cols = c("target_date", "target", "horizon", "location")
    ) %>%
    names()

  expect_equal(sort(names(quantile_outputs)), sort(output_names))
})


test_that("invalid output type throws error", {
  expect_error(
    quantile_outputs %>%
      dplyr::mutate(output_type = "median") %>%
      linear_pool()
  )
})


test_that("weights column already in quantile_outputs generates error", {
  expect_error(
    quantile_outputs %>%
      dplyr::mutate(weight = "a") %>%
      linear_pool(weights = fweight)
  )
})
