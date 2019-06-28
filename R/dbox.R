#' Combined density/box plots.
#'
#' @param x A continuous numeric vector.
#' @param w A vector of weights. Must be either NULL (the default) or the
#'   same length as <x>.
#' @param coef Defines the length of boxplot whiskers & outliers. Defaults
#'   to the typical stat_boxplot value of 1.5.
#' @param color Fill and line color of both density and box plots.
#' @param alpha Fill transparency of both density and box plots.
#' @param lwt Weight of density plot outline.
#' @param ltype Line type (pattern) of density plot outline. Default is solid.
#'   For other options, see ?linetype.
#' @param fill Logical. Fill the density plot? Gets color from <color> and
#'   alpha from <alpha>. Defaults to TRUE
#' @param normal Logical. Include a simulated normal/Gaussian density
#'   plot? Gets parameters from <x> Defaults to FALSE.
#' @param color_norm Line color of the optional simulated normal plot.
#' @param lwt_norm Weight of the optional simulated normal plot.
#' @param ltype_norm Line type simulated normal. Default is twodash.
#' @import ggplot2
#' @importFrom stats density rnorm sd quantile IQR
#' @export
#' @examples
#' ndensity <- rnorm(5000)
#' dbox(ndensity)
#'
#' rskew <- 1.5^rnorm(5000) * 5
#' dbox(rskew, color = "orange2", fill = FALSE,
#'      normal = TRUE, color_norm = "steelblue2")


dbox <- function(x,
                  w = NULL,
                  coef = 1.5,
                  color = "black",
                  alpha = 0.2,
                  lwt = 1.1,
                  ltype = "solid",
                  fill = TRUE,
                  normal = FALSE,
                  color_norm = "black",
                  lwt_norm = 1.1,
                  lytpe_norm = "twodash"
                  ) {

  # equal weights if not supplied
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1 / nx, nx)
  }

  # compute density & create data frame
  dens <- density(x, weights = w, na.rm = TRUE)
  ddf <- data.frame(
    x = dens$x,
    density = dens$y
  )


  # compute outliers
  iqr <- stats::IQR(x, na.rm = TRUE)
  outliers <- x[x < (quantile(x, 0.25, na.rm = TRUE) - coef * iqr) |
                  x > (quantile(x, 0.75, na.rm = TRUE) + coef * iqr)]


  # compute comparison simulated normal distribution & data frame
  norm <- rnorm(length(x), mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  ndens <- density(norm)
  ndf <- data.frame(
    x = ndens$x,
    density = ndens$y
  )

  # variable name as character string
  varname <- deparse(substitute(x))


  ### INDIVIDUAL PLOT ELEMENTS ###

  # desnity line plot
  d <- geom_path(
    data = ddf,
    aes(x = density, y = x, color = varname),
    size = lwt,
    linetype = ltype
  )

  # density area plot
  a <- geom_polygon(
    data = ddf,
    aes(x = density, y = x),
    fill = color,
    color = NA,
    alpha = alpha
  )

  # boxplot
  b <- stat_boxplot(
    data = NULL,
    aes(y = x),
    coef = coef,
    fill = color,
    color = color,
    alpha = alpha,
    outlier.shape = NA,
    width = .2 * max(ddf$density),
    position = position_nudge(x = -0.15 * max(ddf$density))
  )

  # outliers
  o <-  geom_point(
    aes(y = outliers, x = -0.15 * max(ddf$density)),
    color = color,
    alpha = alpha,
    position = position_jitter(width = .075 * max(ddf$density))
  )

  # optional normal density comparison plot
  n <- geom_path(
    data = ndf,
    aes(x = density, y = x, color = "Simulated Normal"),
    size = lwt_normal,
    linetype = ltype_normal
  )

  # scale
  s <- scale_color_manual(name = "", values = c(color, color_norm))

  # theme
  t <- theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    )




  ### CONDITIONAL PLOTTING ###

  ggplot() +
    {if (normal) n} +
    {if (fill) a} +
    {if (length(outliers) > 0) o} +
    d + b + s + t + coord_flip()




}


