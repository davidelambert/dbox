#' Test Data for Distribution Visualizations
#'
#' A set of random distributions for visualization tests, genereated by
#' densitybox::test_dists(). For for details see documentation there.
#'
#' @format A data frame of 10,000 observations and 7 variables.
#' \describe{
#'   \item{normal}{Random normal}
#'   \item{rskew}{Right-skewed through exponentiation}
#'   \item{lskew}{Left-skewed through log transformation & exponentiation}
#'   \item{uniform}{Uniform on [-3,3]}
#'   \item{bimodal}{Bimodal sampled from rskew & lskew}
#'   \item{sine}{Sampled from absolute value sine function}
#'   \item{weird}{Concatenated Chi-squared distributions with random parameters}
#' }
'dbox_dists'


#' Guilford County, NC 2017 Income Data
#'
#' 3,352 observations plus survey weights of 2017 Guilford County, NC personal
#' income. Source: American Community Survey, via IPUMS USA.
#'
#' @format A data frame of 3,352 observations and 2 variables.
#' \describe{
#   \item{income}{Personal income in USD for persons age 16+ with income.}
#   \item{perwt}{Person weight: unscaled survey weight for a 1% sample.}
#' }
'guilford'
