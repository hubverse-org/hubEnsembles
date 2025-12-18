#' Function that accepts weights and performs an exact weighted quantile
#' computation from a vector of samples
#'
#' @param x numeric vector from which to calculate (weighted) sample quantiles
#' @param weights numeric vector of weights for each `x` value
#' @param normalize boolean specifying whether to normalize the `weights`.
#'   Defaults to TRUE
#' @param probs numeric vector of (quantile) probabilities
#' @noRd
#'
#' @return a vector of weighted quantile values for the requested quantile levels

weighted_quantile <- function(
  x,
  weights = NULL,
  normalize = TRUE,
  probs = c(0, .25, .5, .75, 1)
) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be numeric.")
  }

  if (is.null(weights)) {
    weights <- rep(1, length(x))
    normalize <- TRUE
  }
  if (!is.numeric(weights)) {
    cli::cli_abort("{.arg weights} must be numeric.")
  }

  if (length(x) != length(weights)) {
    cli::cli_abort("'x' and 'weights' must have the same length.")
  }

  # if normalize = TRUE, normalize the weights
  if (normalize) {
    weights <- weights / sum(weights)
  }

  # check that the sum of weights is 1
  if (!isTRUE(all.equal(sum(weights), 1, tolerance = 1e-3))) {
    cli::cli_abort("{.arg weights} must sum to 1 or be normalized")
  }

  # check that probs is numeric, and all are >= 0 and <= 1
  if (!is.numeric(probs) || any(probs < 0 | probs > 1)) {
    cli::cli_abort("{.arg probs} must be numeric and between 0 and 1.")
  }

  # sort both x and weights in increasing order of x
  x_order <- order(x)
  x_sorted <- x[x_order]
  weights_sorted <- weights[x_order]

  # cumulative weights
  cum_weights <- cumsum(weights_sorted)

  # quantiles at each probability level:
  # the smallest x with cumulative weight at least as large as the desired probability level
  quantiles <- lapply(
    probs,
    function(prob) {
      x_sorted[min(which(cum_weights >= prob))]
    }
  ) |>
    unlist()

  return(quantiles)
}
