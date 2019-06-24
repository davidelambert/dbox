#' Generate a dataframe of several random distributions for visualization tests.
#' Distributions include: normal, right- and left-skewed, uniform, bimodal,
#' absolute value sine, and a "weird" distribution of concatenated
#' chi-squared's with random parameters.
#'
#' @param n Number of observations.
#' @import stats
#' @export
#' @examples
#' testing <- test_dists(5000)


test_dists <- function(n) {
  normal <- rnorm(n) / 1.1
  rskew <- 1.5^(rnorm(n)) * 5
  lskew <- log(rnorm(n)^2)
  bimodal <- c(sample(lskew, n/2), sample(rskew, n/2))
  uniform <- runif(n, -3, 3)
  sine <- abs(sin(seq(-pi, pi, 2*pi/n)))
  sine <- sine[1:n]
  weird <- NULL
  while (length(weird) < n) {
    weird <- append(
      weird,
      rchisq(
        sample(1:(100), 1),
        sample(1:5000, 1),
        sample(1:500, 1)
      ) ^ (1/(sample(3:9, 1)))
    )
  }
  weird <- weird[1:n]

  dists <- data.frame(
    normal,
    rskew,
    lskew,
    uniform,
    bimodal,
    sine,
    weird
  )

  return(dists)
}
