#' @title Linear
#'
#' @description
#' This function takes in a domain vector x, and scalars to indicate slope, intercept, lower bound a, and upper bound b, and
#' returns a vector y equal to (slope * x + intercept) for a <= x <= b, and zero otherwise, defined over domain x.
#'
#'
#' @param x         // Domain vector
#' @param slope     // Scalar to indicate slope
#' @param intercept // Scalar to indicate x-intercept
#' @param a         // Lower bound scalar
#' @param b         // Upper bound scalar

#' @returns returns a vector y
#' @export
#'
#' @examples
#' # Generating y = -.5x + .5 for -1 <= x <= 1 over the domain (-3, 3)
#'
#' x <- seq(-3, 3, by = .002)
#' y <- linear(x, -.5, -1, 1)

linear <- function(x, slope, intercept, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  y_int <- y * intercept
  y <- y * x
  y <- y * slope + y_int
  return(y)
}
