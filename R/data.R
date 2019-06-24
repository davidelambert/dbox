#' Test Data for Distribution Visualizations
#'
#' A set of random distributions for visualization tests, genereated by
#' densitybox::test_dists(). For for details see documentation there.
#'
#' @format A data frame of 10,000 observations and 7 variables.
#' \describe{
#' \item{normal}{Random normal}
#' \item{rskew}{Right-skewed through exponentiation}
#' \item{lskew}{Left-skewed through log transformation & exponentiation}
#' \item{uniform}{Uniform on [-3,3]}
#' \item{bimodal}{Bimodal sampled from rskew & lskew}
#' \item{sine}{Sampled from absolute value sine function}
#' \item{weird}{Concatenated Chi-squared distributions with random parameters}
#' }
"dbox_dists"
