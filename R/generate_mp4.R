#' @title Generate .mp4
#'
#' @description
#' This function Generates a .mp4 file of a visual representation of the convolution of the input function with itself. If nIter is greater than 1,
#' then at the end of each iteration, the function is replaced with the convolution, with the kernel remaining constant. Frequency and fps
#' determine the number of total plot frames and the frame rate of the .mp4, respectively. Bound and step size indicate the radius of the plot
#' and the distance between values over which the input function is calculated.
#'
#' @param input_function         // Vector of function values for use as base function and kernel
#' @param bound                  // Radius of the x-axis for the plot
#' @param step_size              // Distance between each value in the input function
#' @param freq                   // Dictates the number of indices to skip before each frame is generated
#' @param fps                    // Frame rate for the .mp4 file
#' @param nIter                  // Number of convolutions to generate in the .mp4 file
#'
#'
#' @import magic
#' @import magick
#' @import av
#' @import ggplot2
#'
#' @examples
#' # Generating a one loop .mp4 using a uniform distribution on (-.5,.5), with step size equal to 1/2,
#' # 4 indices skipped, and a frame rate of 30.
#'
#' x <- seq(-2, 2, by = .5)
#'
#' y <- unif(x, -.5, .5)
#'
#' generate_mp4(input_function = y, bound = 2, step_size = .5, freq = 4, fps = 30, nIter = 1)
#' @export
generate_mp4 <- function(input_function = NULL, bound = 2, step_size = .002, freq = 4, fps = 30, nIter = 1){

  # Create a and b variables based off of bound
  a <- -1* bound
  b <- 1 * bound

  # If no function is input, then default to a uniform distribution
  if(is.null(input_function)){

    x <- seq(a, b, by = step_size)

    input_function <- unif(x, -.5, .5)

  }

  # Compatibility check.
  if(length(input_function) != (2 * bound)/step_size + 1){
    warning("Input function should be defined over the domain vector: x <- seq(-bound, bound, by = step_size)")
  }

  # Create bounds to do "off - screen" calculations
  a1 <- 2*a
  b1 <- 2*b

  # Calculate corresponding indices for a and b
  a_plot <- (a - a1)/step_size
  b_plot <- (b - a1)/step_size

  # Calculate indices to begin image capturing
  start <- (a - a1)/(2*step_size)
  end <- ((b + b1)/2 - a1)/step_size

  # Create a domain including the adjusted bounds
  x_vals <- seq(a1, b1, by = step_size)

  # Create a vector to store the convolution of the function and the kernel
  conv <- rep(0, length(x_vals))

  # Create a vector to store the function over the adjusted domain
  func <- rep(0, length(x_vals))

  # Set the value of the function equal to the input function
  func[a_plot:b_plot] <- input_function

  # The kernel is equal to the input function, adjusted to the lowest point of the domain.
  ker <- func
  ker <- ashift(ker, -(length(x_vals)-1)/2)
  index <- 1

  # Initialize a vector to store the generated images
  plot_files <- character((end - start) / freq * nIter)

  for(j in 1:nIter){

    for(i in 1:length(x_vals)){

      # Shift the kernel to the right by one index
      ker <- ashift(ker, 1)

      # Calculate the convolution value ad the current index
      conv[i] <- convolution(func, ker, (1/step_size))

      # Plotting only occurs within the bounds of the start and end indices. This is to reduce the number of frames where nothing changes.
      # Plotting skips indices according to freq parameter to reduce run time.
      if((i-1) %% freq == 0 && i >= start && i <= end){

        # Scale the vectors to the desired dimensions of the plot
        x_graph <- x_vals[a_plot:b_plot]
        func_graph <- func[a_plot:b_plot]
        ker_graph <- ker[a_plot:b_plot]
        conv_graph <- conv[a_plot:b_plot]

        # Create data frame for the plot
        data <- data.frame(
          x = rep(x_graph, 3),
          y = c(func_graph, ker_graph, conv_graph),
          function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_graph))
        )

        # Generate the plot (a single frame for the .mp4)
        plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
          geom_line(size = 1) +
          labs(title = "Compound Convolution of Distributions", x = "X", y = "Density") +
          scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
          theme_minimal() +
          theme(
            panel.background = element_rect(fill = "white", color = "white"),
            plot.background = element_rect(fill = "white", color = "white")
          ) +
          ylim(0,2)

        # Save the image
        plot_file <- paste0("plot_", index, ".png")
        ggsave(plot_file, plot = plot, width = 5, height = 4)

        plot_files[index] <- plot_file

        # Update index
        index <- index + 1

      }

    }

    # Replace the function with the computed convolution for the next iteration
    func <- conv

    # Reset the kernel vector
    ker <- ker - ker
    ker[a_plot:b_plot] <- input_function
    ker <- ashift(ker, -(length(x_vals)-1)/2)

    # Reset the convolution vector
    conv <- conv - conv

  }

  # Set frame rate for .mp4 file
  framerate <- fps

  # Generate the video using the plot files
  av_encode_video(plot_files, output = "compound_convolution.mp4", framerate = framerate)

  # Delete the .png files
  file.remove(plot_files)
}
