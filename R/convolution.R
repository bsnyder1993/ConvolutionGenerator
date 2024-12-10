#' Convolution
#'
#' @param f        // Function vector corresponding to f(x)
#' @param g        // Function vector corresponding to g(x - t)
#' @param width     // The length of the interval over which the convolution is being calculated
#'
#' @returns returns a scalar equal to the average value of f(x)*g(x - t)
#'
#' @examples
#' # Generating a function, a kernel equal to the function shifted 50 indices to the right, and discretely calculating the value of the area under function*kernel
#'
#' x <- seq(-3, 3, by = .002)
#'
#' func <- absolute(x, 1, -1, 1)
#'
#' convolution_value <- convolution(func, kernel, length(function))
#'
#' @export
convolution <- function(f, g, width){
  area <- sum(f * g)
  return(area / width)
}
