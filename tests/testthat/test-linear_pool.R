library(Hmisc)
library(distfromq)
library(matrixStats)
library(dplyr)
library(tidyr)

test_that("non-default columns are dropped from output", {
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

  cdf_outputs <- mutate(quantile_outputs, output_type="cdf")

  output_names <- quantile_outputs %>%
    dplyr::mutate(extra_col_1 = "a", extra_col_2 = "a") %>%
    linear_pool(
      task_id_cols = c("target_date", "target", "horizon", "location")
    ) %>%
    names()

  expect_equal(sort(names(quantile_outputs)), sort(output_names))
})




test_that("(weighted) quantiles correctly calculated", {
  # The three component models provide quantiles from the distributions
  # F_1 = N(-3, 1), F_2 = N(0,1), and F_3 = N(3, 1)
  # The linear pool is a (weighted) mixture with cdf F(x) = \sum_m w_m F_m(x)
  # We test with equal weights w_m = 1/3 and with weights w_1 = 0.25, w_2 = 0.5, w_3 = 0.25
  quantile_expected <- weighted_quantile_expected <- data.frame(
    stringsAsFactors = FALSE,
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
  
  component_outputs <- expand.grid(
    stringsAsFactors = FALSE,
    model_id = letters[1:3],
    location = "111",
    horizon = 1, 
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    output_type = "quantile",
    output_type_id = output_prob,
    value = NA_real_)

  component_outputs$value[component_outputs$model_id == "a"] <- 
    stats::qnorm(output_prob, mean=-3)
  component_outputs$value[component_outputs$model_id == "b"] <- 
    stats::qnorm(output_prob, mean=0)
  component_outputs$value[component_outputs$model_id == "c"] <- 
    stats::qnorm(output_prob, mean=3)
                       
  weighted_component_outputs <- expand.grid(
    stringsAsFactors = FALSE,
    model_id = letters[1:3],
    location = "111",
    horizon = 1, 
    target = "inc death",
    target_date = as.Date("2021-12-25"),
    output_type = "quantile",
    output_type_id = weighted_output_prob,
    value = NA_real_)

  weighted_component_outputs$value[weighted_component_outputs$model_id == "a"] <- 
    stats::qnorm(weighted_output_prob, mean=-3)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "b"] <- 
    stats::qnorm(weighted_output_prob, mean=0)
  weighted_component_outputs$value[weighted_component_outputs$model_id == "c"] <- 
    stats::qnorm(weighted_output_prob, mean=3)
    
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
               tolerance=1e-3)
  expect_equal(weighted_quantile_expected,
               as.data.frame(weighted_quantile_actual),
               tolerance=1e-3)
})

