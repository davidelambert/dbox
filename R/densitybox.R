#' Combined desnity/box plot with optional simulated normal density plot.
#'
#' @param x A continuous numeric vector.
#' @param color Fill and line color of both density and box plots.
#' @param alpha Fill transparency of both density and box plots.
#' @param linewt Weight of density plot outline.
#' @param linewt_box Weight of boxplot lines.
#' @param normal Logical. Include a simulated normal/Gaussian density line plot with the same parameters as x? Defaults to FALSE.
#' @param color_norm Line color of the optional simulated normal plot.
#' @param linewt_norm Wieght of the optional simulated normal plot.
#' @import ggplot2
#' @export
#' @examples
#' ndensity <- rnorm(5000)
#' densitybox(ndensity)

densitybox <- function(x,
                       color = "black",
                       alpha = 0.2,
                       linewt = 1.1,
                       linewt_box = 0.8,
                       normal = FALSE,
                       color_norm = "black",
                       linewt_norm = 1.1
                       ) {

  bpstats <- as.vector(boxplot(x, plot = FALSE)$stats)
  outliers <- boxplot(x, plot = FALSE)$out
  ymin <- -(max(density(x)$y)/20)
  ymax <- -(max(density(x)$y)/4)
  varname <- deparse(substitute(x))

  p <- ggplot() +
    # density fill
    geom_density(
      aes(x = x),
      fill = color,
      color = NA,
      alpha = 0.2
    ) +
    # density upper border
    stat_density(
      geom = "line",
      aes(x = x, color = varname),
      size = linewt
    ) +
    # boxplot central box
    annotate(
      "rect",
      xmin = bpstats[2],
      xmax = bpstats[4],
      ymin = ymin,
      ymax = ymax,
      fill = color,
      alpha = 0.2,
      color = color,
      size = linewt_box
    ) +
    # boxplot median line
    geom_segment(
      data = data.frame(x = x),
      x = bpstats[3],
      xend = bpstats[3],
      y = ymin,
      yend = ymax,
      color = color,
      size = linewt_box
    ) +
    # boxplot left whisker
    geom_segment(
      data = data.frame(x = x),
      x = bpstats[1],
      xend = bpstats[2],
      y = (ymax + ymin) / 2,
      yend = (ymax + ymin) / 2,
      color = color,
      size = linewt_box
    ) +
    # boxplot right whisker
    geom_segment(
      data = data.frame(x = x),
      x = bpstats[4],
      xend = bpstats[5],
      y = (ymax + ymin) / 2,
      yend = (ymax + ymin) / 2,
      color = color,
      size = linewt_box
    ) +
    # theme
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom"
    )

  # outliers, if any
  o <- geom_jitter(
    data = data.frame(x = outliers),
    aes(
      x = outliers,
      y = (ymax + ymin) / 2
    ),
    color = color,
    alpha = 0.2,
    height = abs((ymax + ymin) / 5)
  )

  # normal density comparison, optional
  n <- stat_density(
    geom = "line",
    aes(x = rnorm(length(x), mean(x), sd(x)),
      color = "simulated normal"
    ),
    linetype = "twodash",
    size = linewt_norm
  )

  # conditional plotting
  if (normal == TRUE & length(outliers != 0)) {
    p + n + o + scale_color_manual(name = "", values = c(color, color_norm))
  } else if (normal == FALSE & length(outliers != 0)) {
    p + o + scale_color_manual(name = "", values = c(color))
  } else if (normal == TRUE & length(outliers == 0)) {
    p + n + scale_color_manual(name = "", values = c(color, color_norm))
  } else
    p + scale_color_manual(name = "", values = c(color))

}
