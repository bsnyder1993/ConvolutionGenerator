#' @title Absolute
#'
#' @description
#' This function takes in a domain vector x, a slope scalar val, a lower bound a, and an upper bound b and generates
#' a vector y equal to (val * |x|) when a a <= x <= b, and zero otherwise.
#'
#'
#' @param x          // Domain vector
#' @param val        // Scalar to determine slope
#' @param a          // Lower bound scalar
#' @param b          // Upper bound scalar


#' @returns returns a vector y
#' @export
#'
#' @examples
#' # Creating vector y equal to |x| for -1 <= x <= 1, and zero otherwise over the domain (-3, 3)
#'
#' x <- seq(-3, 3, by = .002)
#' y <- absolute(x, 1, -1, 1)

absolute <- function(x, val, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  y <- y * x
  y <- abs(y) * val
  return(y)
}
