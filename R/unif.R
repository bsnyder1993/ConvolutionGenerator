#' @title Uniform
#'
#' @description
#' This function takes in a domain vector x, a lower bound a, and an upper bound b, and returns a
#' vector y equal to uniform(a,b) defined over x.
#'
#'
#' @param x         // Domain vector
#' @param a         // Lower bound scalar
#' @param b         // Upper bound scalar
#' @returns returns a vector y
#'
#' @examples
#' # Creating a uniform(-.5,.5) vector over the domain (-2, 2)
#'
#' x <- seq(-2, 2, by = .002)
#' y <- unif(x, -.5, .5)
#' @export
unif <- function(x, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  return(y / (b - a))
}
