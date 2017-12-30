#' Approximate Bayesian Computation
#' @param data0 The data
#' @param fun The model
#' @param epsilon Convergence tolerance
#' @param iter Maximum number of iterations
#' @param summary_fun Summary statistics of the data and simulated data
#' @param distance_fun Distance between two summary statistics
#' @examples
#' data0 <- rgamma(5000, 5, 2)
#' generative_fun <- function(alpha, beta) {rgamma(5000, alpha, beta)}
#' abc(data0, generative_fun)
#' @export
abc <- function(data0, fun, epsilon = 1e-6, iter = 1000,
                summary_fun = c(mean, var), distance_fun = l2_norm) {
  eval_fun <- . %>% { do.call(fun, as.list(.)) }
  perf_fun <- function(x, y) { metric(x, y, summary_fun, distance_fun) }
  has_warning_or_error <- function(x) {
    any(c('warning', 'error') %in% class(x))
  }
  param_dim <- length(formalArgs(fun))

  count <- 0
  p <- runif(param_dim)
  current_distance <- perf_fun(eval_fun(p), data0)
  while ((current_distance > epsilon) && (count < iter)) {
    count <- count + 1
    new_p <- p + rnorm(param_dim, sd = 0.1)
    new_distance <- tryCatch(
      perf_fun(eval_fun(new_p), data0),
      error = identity, warning = identity
    )
    if (!has_warning_or_error(new_distance)) {
      if (new_distance < current_distance) {
        p <- new_p
        current_distance <- new_distance
        # cat("Improved!", p, current_distance, "\n")
      }
    }
  }
  list(param = p, distance_from_data = current_distance, iter = count)
}
