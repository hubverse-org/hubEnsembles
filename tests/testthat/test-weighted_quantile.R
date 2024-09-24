test_that("non-numeric x throws an error", {
  x <- rep("1", 5)
  probs <- seq(from = 0.1, to = 0.9, by = 0.2)

  weighted_quantile(x, weights = NULL, normalize = FALSE, probs) |>
    expect_error(
      regexp = "`x` must be numeric", fixed = TRUE
    )
})

test_that("non-numeric weights throws an error", {
  x <- seq(from = 1, to = 16, by = 3)
  weights <- rep("1", length(x))
  probs <- seq(from = 0.1, to = 0.9, by = 0.2)

  weighted_quantile(x, weights, normalize = TRUE, probs) |>
    expect_error(
      regexp = "`weights` must be numeric", fixed = TRUE
    )
})

test_that("weights that don't sum to 1 and aren't normalized throw an error", {
  x <- seq(from = 1, to = 16, by = 3)
  weights <- rep(1, length(x))
  probs <- seq(from = 0.1, to = 0.9, by = 0.2)

  weighted_quantile(x, weights, normalize = FALSE, probs) |>
    expect_error(
      regexp = "`weights` must sum to 1 or be normalized", fixed = TRUE
    )
})

test_that("invalid probs (quantile levels) throws an error", {
  x <- seq(from = 1, to = 16, by = 3)
  probs <- seq(from = -0.25, to = 1, by = 0.25)

  weighted_quantile(x, weights = NULL, normalize = TRUE, probs) |>
    expect_error(
      regexp = "`probs` must be numeric and between 0 and 1", fixed = TRUE
    )
})

test_that("quantiles are correctly calculated", {
  x <- c(1, 4.2, 9, 14)
  weights <- c(4, 2, 1, 3)
  probs <- c(0.25, 0.5, 0.75)

  expected_quantiles <- c(1, 4.2, 14)
  actual_quantiles <- weighted_quantile(x, weights, normalize = TRUE, probs)
  expect_equal(actual_quantiles, expected_quantiles)
})
