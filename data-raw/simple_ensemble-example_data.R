# create example model_outputs
model_outputs <- expand.grid(
  stringsAsFactors = FALSE,
  model_id = letters[1:4],
  location = c("222", "888"),
  horizon = 1, #week
  target = "inc death",
  target_date = as.Date("2021-12-25"),
  output_type = "quantile",
  output_type_id = c(.1, .5, .9),
  value = NA_real_
)

model_outputs$value <- c(
  10,
  30,
  15,
  20,
  100,
  300,
  400,
  250,
  40,
  40,
  45,
  50,
  150,
  325,
  500,
  300,
  60,
  70,
  75,
  80,
  250,
  350,
  500,
  350
)


# create example weights
fweights <- expand.grid(
  stringsAsFactors = FALSE,
  model_id = letters[1:4],
  location = c("222", "888"),
  weight = NA_real_
)
fweights$weight <- c(seq(0.1, 0.4, 0.1), seq(0.4, 0.1, -0.1))

save(model_outputs, file = "data/simple_ensemble-example_outputs.rda")
save(fweights, file = "data/simple_ensemble-example_weights.rda")
