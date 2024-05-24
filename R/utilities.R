#' Calibration Coefficients
#'
#' @param x axis input values from plot
#' @param y axis output values from image
#' @param log logical indicating if axis is logged
#'
#' @return numeric vector of length 2
calibration_coefs <- function(x, y, log = FALSE) {
  if (log) {
    # solve y = b * 10^(m * x)
    m <- log(y[1] / y[2], 10) / (x[1] - x[2])
    b <- y[1] / 10^(m * x[1])
    c(b, m)
  } else {
    # solve y = b + m * x
    x <- matrix(c(1, 1, x), ncol = 2)
    y <- matrix(y, ncol = 1)
    c(solve(crossprod(x), crossprod(x, y)))
  }
}

#' Scale
#'
#' @param x input values
#' @param b calibration coefficients
#' @param log logical indicating if axis is logged
#' @param reverse logical indicating to go from plot to image values (FALSE)
#' or image to plot values (TRUE)
#'
#' @return numeric vector with same length as x
tolp_scale <- function(x, b, log = FALSE, reverse = FALSE) {
  if (reverse) {
    if (log) {
      log(x / b[[1]], 10) / b[[2]]
    } else {
      (x - b[[1]]) / b[[2]]
    }
  } else if (log) {
    b[[1]] * 10^(b[[2]] * x)
  } else {
    b[[1]] + b[[2]] * x
  }
}

#' TOLP Resource
#'
#' @param ... character vectors, specifying subdirectory and file(s) within the
#' tolp package. The default, none, returns the root of the package. Wildcards
#' are not supported.
#'
#' @return the contents of the resource as a string
tolp_resource <- function(...) {
  paste(readLines(system.file(..., package = "tolp")), collapse = "\n")
}
