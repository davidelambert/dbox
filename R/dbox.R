#' Combined density/box plots.
#'
#' @param x A continuous numeric vector.
#' @param w A vector of weights. Must be either NULL (the default) or the
#'   same length as <x>.
#' @param coef Defines the length of boxplot whiskers & outliers. Defaults
#'   to the typical stat_boxplot value of 1.5.
#' @param normal Logical. Include a simulated normal/Gaussian density
#'   plot? Gets parameters from <x> Defaults to FALSE.
#' @param label A character string supplying a variable name to the legend.
#'   The default, NULL, passes the argument supplied to <x> to the legend.
#' @param color Fill and line color of both density and box plots.
#' @param alpha Fill transparency of both density and box plots.
#' @param lwt Weight of density plot outline.
#' @param ltype Line type (pattern) of density plot outline. Default is solid.
#'   For other options, see ?linetype.
#' @param fill Logical. Fill the density plot? Gets color from <color> and
#'   alpha from <alpha>. Defaults to TRUE
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
                 normal = FALSE,
                 label = NULL,
                 color = "black",
                 alpha = 0.2,
                 lwt = 1.1,
                 ltype = 1,
                 fill = TRUE,
                 color_norm = "black",
                 lwt_norm = 1.1,
                 ltype_norm = 6
                 ) {


  # variable name as character string
  varname <- ifelse(is.null(label),
                    deparse(as.list(sys.call())[[2]]),
                    label)

  # run on complete cases only
  xc <- na.omit(x)

  # equal weights if not supplied
  if (is.null(w)) {
    w <- rep(1 / length(xc), length(xc))
  }

  # compute density & create data frame
  dens <- density(xc, weights = w, na.rm = TRUE)
  ddf <- data.frame(
    x = dens$x,
    density = dens$y
  )


  # compute outliers
  iqr <- stats::IQR(xc)
  outliers <- xc[xc < (quantile(xc, 0.25) - coef * iqr) |
                 xc > (quantile(xc, 0.75) + coef * iqr)]


  # compute comparison simulated normal distribution & data frame
  norm_x <- seq(-4, 4, length.out = 512) * sd(xc) + mean(xc)
  norm_y <- dnorm(norm_x, mean(xc), sd(xc))
  ndf <- data.frame(
    x = norm_x,
    density = norm_y
  )



  ### INDIVIDUAL PLOT ELEMENTS ###

  # desnity line plot
  d <- geom_path(
    data = ddf,
    aes(x = density, y = x, color = varname, linetype = varname),
    size = lwt
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
    aes(y = xc),
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
    aes(x = density, y = x,
        color = "Simulated Normal", linetype = "Simulated Normal"),
    size = lwt_norm
  )

  # scales for legend, conditional on <normal>
  if (normal) {
    sc <- scale_color_manual(name = "",
                             values = c(color_norm, color))
    sl <- scale_linetype_manual(name = "",
                                values = c(ltype_norm, ltype))
  } else {
    sc <- scale_color_manual(name = "", values = color)
    sl <- scale_linetype_manual(name = "", values = ltype)
  }

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
    d + b + sc + sl + t + coord_flip()




}


