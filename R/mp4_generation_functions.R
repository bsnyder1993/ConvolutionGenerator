library(ggplot2)
library(magick)
library(av)

#' @param interval          # a vector of the domain for the plots
#' @param func              # a vector of values of a function over the domain
#' @param step_size         # an integer value of the size of each step
#' @param precision         # the difference in each interval value
#' @param nIter             # the number of compounded convolutions to perform
#' @param

#' @returns                 # saves a .mp4 file in the
#' @export
#'
#' @examples
#' # Calling the LRMultiClass function on a 10 x 101 Matrix X, with first column all ones,
#' # and a vector of class assignments Y
#'

# Internal function to generate a uniform pulse from a to b over domain x
unif <- function(x, val, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a)-as.numeric(x > b)
  return(list(vec = y, lower = a, upper = b))
}

# Internal function to compute the area under the intersection of the kernel and the function
convolution <- function(x, kernel, func){
  area <- sum(pmin(kernel, func))
  return(area / 500)
}

# Main function to generate .mp4 file
generate_mp4_uniform_kernel <- function(interval, func, step_size, precision, nIter = 1){

  # This is currently hardcoded, I need to add a function to calculate the number based on interval length and step size
  plot_files <- character(501*nIter)

  # Create a vector to store the convolution value
  conv <- rep(0, length(interval))

  for(j in 1:nIter){

    for(i in 1:501){

      # center is currently hardcoded for the interval (-2,2)
      # this needs to be changed to be calculated based off of inputs
      center <- -2.5 + .01 * (i - 1)

      # function currently assumes the kernel is always uniform
      y2 <- unif(x_vals, 1, center - .5, center + .5)

      # Generate an index for computing the convolution
      # This is currently hardcoded for interval length of 2001 and step size of 5
      index <- 5*(i-1) + 1 - 250
      index <- min(index, 2001)

      y3[index] <- convolution(x, y1$vec, y2$vec)

      # This fills in the values in between steps
      # This is currently hardcoded for step size of 5, this needs to be generalized
      if(i > 2){
        y3[index - 4] = y3[index - 5] + (y3[index] - y3[index - 5])*1/5
        y3[index - 3] = y3[index - 5] + (y3[index] - y3[index - 5])*2/5
        y3[index - 2] = y3[index - 5] + (y3[index] - y3[index - 5])*3/5
        y3[index - 1] = y3[index - 5] + (y3[index] - y3[index - 5])*4/5
      }

      # Create data frame from func, kernel, and convolution
      data <- data.frame(
        x = rep(x_vals, 3),
        y = c(y1$vec, y2$vec, y3),
        function_type = rep(c("Blah", "Kernel", "Wowwee"), each = length(x_vals))
      )

      # Plot the data
      plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
        geom_line(size = 1) +
        labs(title = "Lets go boys", x = "X", y = "Density") +
        scale_color_manual(values = c("blue", "red", "black")) +  # Set custom colors
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = "white"),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        ylim(0,2)

      # Save plot
      plot_file <- paste0("plot_", i + 501 * (j - 1), ".png")
      ggsave(plot_file, plot = plot, width = 5, height = 4)

      plot_files[i + 501 * (j - 1)] <- plot_file

    }

  }

  # Generate the .mp4 file
  # Currently the name of the file and framerate are hardcoded, this needs to be adjusted.

  framerate <- 30

  av_encode_video(plot_files, output = "convolution_vid.mp4", framerate = framerate)

  file.remove(plot_files)

}
