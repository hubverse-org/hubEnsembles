# Create quantiles from three component models that are normal distributions with
# means -3, 0, and 3, all standard deviations 1, and weights 0.25, 0.5, and 0.25.
library(purrr)

component_ids <- letters[1:3]
component_weights <- c(0.25, 0.5, 0.25)
component_means <- c(-3, 0, 3)

lp_qs <- seq(from = -5, to = 5, by = 0.25) # linear pool quantiles, expected outputs
ps <- rep(0, length(lp_qs))
for (m in seq_len(3)) {
  ps <- ps + component_weights[m] * pnorm(lp_qs, mean = component_means[m])
}

component_qs <- purrr::map(component_means, ~ qnorm(ps, mean = .x)) |> unlist()
component_outputs <- data.frame(
  stringsAsFactors = FALSE,
  model_id = rep(component_ids, each = length(lp_qs)),
  target = "inc death",
  output_type = "quantile",
  output_type_id = ps,
  value = component_qs
)

weights <- data.frame(model_id = component_ids, weight = component_weights)

save(component_outputs, file = "data/linear_pool-example_outputs.rda")
save(weights, file = "data/linear_pool-example_weights.rda")
