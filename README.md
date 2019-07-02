
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbox

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dbox)](https://cran.r-project.org/package=dbox)
[![Travis build
status](https://travis-ci.org/hwkmth/dbox.svg?branch=master)](https://travis-ci.org/hwkmth/dbox)
<!-- badges: end -->

`dbox` is a package for visualizing continuous distributions with a
combined kernel density plot and box plot. This allows for a
simultaneous view of overall distribution shape in the density plot as
well as important quantiles and extreme values in the box plot. Box plot
outliers are jittered to improve outliers density visualization. An
optional density plot of an idealized normal distribution with the same
parameters is also available for comparison.

## Installation

You can install dbox with: `devtools::install_github('hwkmth/dbox')` and
use `help(dbox)` for more information.

## Examples

``` r
library(dbox)

ndensity <- rnorm(5000)
dbox(ndensity)
```

<img src="man/figures/README-examples-1.png" width="100%" />

``` r

rskew <- 1.5^rnorm(5000) * 5
dbox(rskew, color = "orange2", fill = FALSE,
     normal = TRUE, color_norm = "steelblue2")
```

<img src="man/figures/README-examples-2.png" width="100%" />

``` r

diamonds <- ggplot2::diamonds
diamonds$lprice <- log(diamonds$price)
dbox(diamonds$lprice[diamonds$cut == "Ideal"],
     label = "Log Price: Ideal Cut Diamonds",
     color = "orange2",
     normal = TRUE,
     color_norm = "steelblue2")
```

<img src="man/figures/README-examples-3.png" width="100%" />
