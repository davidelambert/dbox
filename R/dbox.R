#' Combined density/box plots.
#'
#' @param x A continuous numeric vector.
#' @param weights A vector of weights. Must be either NULL (the default) or the
#'   same length as `x`. All complete cases of `x` must have a non-missing
#'   value for `weights`.
#' @param plot Logical, whether to plot immediately. Default is TRUE. If FALSE,
#'   returns the plot, which can be assigned to an object.
#' @param fence_coef Defines the length of boxplot whiskers & defines
#'   outliers. Defaults to the typical value of 1.5.
#' @param normal Logical. Include a simulated normal/Gaussian density
#'   plot? Gets parameters from `x` Defaults to FALSE.
#' @param label A character string supplying a variable name to the legend.
#'   The default, NULL, passes the argument supplied to `x` to the legend.
#' @param color Fill and line color of both density and box plots.
#' @param alpha Fill transparency of both density and box plots.
#' @param lwt Weight of density plot outline.
#' @param ltype Line type (pattern) of density plot outline. Default is solid.
#'   For other options, see ?linetype.
#' @param fill Logical. Fill the density plot? Gets color from `color` and
#'   alpha from `alpha`. Defaults to TRUE
#' @param color_norm Line color of the optional simulated normal plot.
#' @param lwt_norm Weight of the optional simulated normal plot.
#' @param ltype_norm Line type simulated normal. Default is twodash.
#' @import ggplot2
#' @importFrom Hmisc wtd.mean wtd.var wtd.quantile
#' @importFrom stats density rnorm sd quantile IQR
#' @export
#' @examples
#' # basic
#' ndensity <- rnorm(5000)
#' dbox(ndensity)
#'
#' # Using colors & comparison normal density plot
#' rskew <- 1.5^rnorm(5000) * 5
#' dbox(rskew, color = "orange2", fill = FALSE,
#'      normal = TRUE, color_norm = "steelblue2")
#'
#' # Assignment to an object using `plot = FALSE` and using a variable label
#' setosa <- subset(iris, Species == "setosa")
#' setosa_petLn <- dbox(setosa$Petal.Length, plot = FALSE,
#'                      label = "Setosa Petal Width", color = "orange2",
#'                      normal = TRUE, color_norm = "steelblue2")
#' setosa_petLn
#'
#' # Using weights, a wider boxplot fence coefficient, and a thinner line
#' dbox::guilford
#' dbox(guilford$income)
#' dbox(guilford$income,
#'      weights = guilford$perwt,
#'      fence_coef = 2,
#'      lwt = 0.8)


dbox <- function(x,
                 weights = NULL,
                 plot = TRUE,
                 fence_coef = 1.5,
                 label = NULL,
                 color = "black",
                 alpha = 0.2,
                 lwt = 1.1,
                 ltype = 1,
                 fill = TRUE,
                 normal = FALSE,
                 color_norm = "black",
                 lwt_norm = 1.1,
                 ltype_norm = 6
                 ) {

  # variable name as character string
  varname <- ifelse(is.null(label),
                    deparse(as.list(sys.call())[[2]]),
                    label)

  # Input errors/warnings
  if (length(x) == 0) stop("x has no observations")
  if (length(na.omit(x)) == 0) stop("x has no complete cases")
  if (!is.numeric(x)) stop("x must be numeric")
  if (is.factor(x)) {
    warning("x should be a continuous variable. weird results may ensue.")
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) stop("weights must be numeric or NULL")
    if(length(weights) != length(x)) stop("length of weights must equal length of x")
    if (isFALSE(all(complete.cases(x) == complete.cases(weights)))) {
      stop("complete cases of x do not match complete cases of weights
           check missing values")
    }
  }
  if (!is.logical(fill)) stop("please supply TRUE/FALSE to fill")
  if (!is.logical(normal)) stop("please supply TRUE/FALSE to normal")
  if (!is.null(label)) {
    if (!is.character(label)) stop("label must be a character vector (or NULL)")
    if (length(label) > 1) stop("please supply only one label")
  }


  # complete cases only. cases will match, as checked above.
  x <- na.omit(x)
  if(!is.null(weights)) wts <- na.omit(weights)

  # compute density, conditional on whether weights are supplied
  if (is.null(weights)){
    dens <- density(x)
  } else {
    wts_scaled <- wts / sum(wts)
    dens <- density(x, weights = wts_scaled)
  }

  # create density data frame
  ddf <- data.frame(
    x = dens$x,
    density = dens$y
  )


  # determine outliers
  if (is.null(weights)) {
    p25 <- quantile(x, .25)
    p75 <- quantile(x, .75)
  } else {
    p25 <- wtd.quantile(x, weights = wts, probs = 0.25)
    p75 <- wtd.quantile(x, weights = wts, probs = 0.75)
  }

  iqr <- p75-p25

  outs <- x[x < (p25 - fence_coef * iqr) |
            x > (p75 + fence_coef * iqr)]


  # compute comparison simulated normal distribution & data frame
  # to +/- 3.1 standard deviations
  if (is.null(weights)) {
    meanx <- mean(x)
    sdx <- sd(x)
  } else {
    meanx <- wtd.mean(x, weights = wts)
    sdx <- sqrt(wtd.var(x, weights = wts))
  }

  norm_x <- seq(-3.1, 3.1, length.out = 512) * sdx + meanx
  norm_y <- dnorm(norm_x, meanx, sdx)
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

  # density fill/area plot
  if (fill) {
    df <- geom_polygon(
      data = ddf,
      aes(x = density, y = x),
      fill = color,
      color = NA,
      alpha = alpha
    )
  } else {
    df <- NULL
  }


  # boxplot
  if (is.null(weights)) {
    b <- stat_boxplot(
      data = NULL,
      aes(y = x),
      coef = fence_coef,
      fill = NA,
      color = color,
      outlier.shape = NA,
      width = .2 * max(ddf$density),
      position = position_nudge(x = -0.15 * max(ddf$density))
    )
  } else {
    b <- stat_boxplot(
      data = NULL,
      aes(y = x, weight = wts),
      coef = fence_coef,
      fill = NA,
      color = color,
      outlier.shape = NA,
      width = .2 * max(ddf$density),
      position = position_nudge(x = -0.15 * max(ddf$density))
    )
  }


  # boxplot fill
  bf <- annotate(
    geom = "rect",
    xmin = -.05 * max(ddf$density),
    xmax = -.25 * max(ddf$density),
    ymin = p25,
    ymax = p75,
    color = NA,
    fill = color,
    alpha = alpha,
  )

  # outliers
  if (length(outs) > 0) {
    o <-  geom_jitter(
      aes(y = outs, x = -0.15 * max(ddf$density)),
      width = .075 * max(ddf$density),
      height = 0,
      color = color,
      alpha = alpha
    )
  } else {
    o <- NULL
  }


  # optional normal density comparison plot
  if (normal) {
    n <- geom_path(
      data = ndf,
      aes(x = density, y = x,
          color = "Comparison Normal Distribution",
          linetype = "Comparison Normal Distribution"),
      size = lwt_norm
    )
  } else {
    n <- NULL
  }


  # key-value pairs as named vectors for scale constructions
  keys <- c(varname, "Comparison Normal Distribution")
  colorvals <- c(color, color_norm)
  names(colorvals) <- keys
  ltypevals <- c(ltype, ltype_norm)
  names(ltypevals) <- keys

  # scales for legend, conditional on `normal`
  if (normal) {
    sc <- scale_color_manual(name = "", values = colorvals)
    sl <- scale_linetype_manual(name = "", values = ltypevals)
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
      legend.position = "bottom"
    )


  # plot or return
  dboxplot <- ggplot() + df + d + bf + b + n + o + sc + sl + t + coord_flip()

  if (plot) {
    dboxplot
  } else (
    return(dboxplot)
  )

}
