# This file contains the prototype for the mp4 generation as well as testing code
# The relevant code has been properly commented in the mp4_generation_functions.R file
library(ggplot2)
library(magic)
library(magick)
library(av)

unif <- function(x, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  return(y / (b - a))
}

absolute <- function(x, val, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  y <- y * x
  y <- abs(y) * val
  return(y)
}

linear <- function(x, slope, intercept, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a) - as.numeric(x > b)
  y_int <- y * intercept
  y <- y * x
  y <- y * slope + y_int
  return(y)
}

convolution <- function(x, y1, y2, width){
  #area <- sum(pmin(y1, y2))
  area <- sum(y1 * y2)
  return(area / width)
}

first_frame <- function(input_function = NULL, a = -2, b = 2, step_size = .002, freq = 4){

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

  a1 <- x_vals[1]
  b1 <- x_vals[length(x_vals)]

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
  plot_files <- character(1)

  # Outer for loop generates


  for(i in 1:length(x_vals)){

    ker <- ashift(ker, 1)
    conv[i] <- convolution(x, func, ker, (1/step_size))

    if((i-1) %% freq == 0 && i >= start){

      x_graph <- x_vals[a_plot:b_plot]
      func_graph <- func[a_plot:b_plot]
      ker_graph <- ker[a_plot:b_plot]
      conv_graph <- conv[a_plot:b_plot]

      data <- data.frame(
        x = rep(x_graph, 3),
        y = c(func_graph, ker_graph, conv_graph),
        function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_graph))
      )


      plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
        geom_line(size = 1) +
        labs(title = "First Frame", x = "X", y = "Density") +
        scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = "white"),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        ylim(0,2)

      plot_file <- paste0("first_frame.png")
      ggsave(plot_file, plot = plot, width = 5, height = 4)

      plot_files[index] <- plot_file

      index <- index + 1

      break
    }



  }

}

last_frame <- function(input_function = NULL, a = -2, b = 2, step_size = .002, freq = 4){

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

  a1 <- x_vals[1]
  b1 <- x_vals[length(x_vals)]

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
  plot_files <- character(1)

  # Outer for loop generates


  for(i in 1:length(x_vals)){

    ker <- ashift(ker, 1)
    conv[i] <- convolution(x, func, ker, (1/step_size))

    if((i-1) %% freq == 0 && i >= end - freq){

      x_graph <- x_vals[a_plot:b_plot]
      func_graph <- func[a_plot:b_plot]
      ker_graph <- ker[a_plot:b_plot]
      conv_graph <- conv[a_plot:b_plot]

      data <- data.frame(
        x = rep(x_graph, 3),
        y = c(func_graph, ker_graph, conv_graph),
        function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_graph))
      )


      plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
        geom_line(size = 1) +
        labs(title = "Last Frame", x = "X", y = "Density") +
        scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = "white"),
          plot.background = element_rect(fill = "white", color = "white")
        ) +
        ylim(0,2)

      plot_file <- paste0("last_frame.png")
      ggsave(plot_file, plot = plot, width = 5, height = 4)

      plot_files[index] <- plot_file

      index <- index + 1

      break
    }

  }

}

generate_mp4 <- function(input_function = NULL, a = -2, b = 2, step_size = .002, freq = 4, fps = 30, nIter = 3){

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

  a1 <- x_vals[1]
  b1 <- x_vals[length(x_vals)]

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

  # Outer for loop generates
  for(j in 1:nIter){

    for(i in 1:length(x_vals)){

      ker <- ashift(ker, 1)
      conv[i] <- convolution(x, func, ker, (1/step_size))

      if((i-1) %% freq == 0 && i >= start && i <= end){

        x_graph <- x_vals[a_plot:b_plot]
        func_graph <- func[a_plot:b_plot]
        ker_graph <- ker[a_plot:b_plot]
        conv_graph <- conv[a_plot:b_plot]

        data <- data.frame(
          x = rep(x_graph, 3),
          y = c(func_graph, ker_graph, conv_graph),
          function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_graph))
        )


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

        plot_file <- paste0("plot_", index, ".png")
        ggsave(plot_file, plot = plot, width = 5, height = 4)

        plot_files[index] <- plot_file

        index <- index + 1

      }

    }

    func <- conv

    ker <- ker - ker
    ker[a_plot:b_plot] <- input_function
    ker <- ashift(ker, -(length(x_vals)-1)/2)

    conv <- conv - conv

  }

  framerate <- 30

  av_encode_video(plot_files, output = "compound_convolution.mp4", framerate = framerate)

  file.remove(plot_files)
}

x <- seq(-1.5, 1.5, by = .002)
func <- absolute(x, 1, -1, 1)

first_frame(func, -1.5, 1.5, step_size = .002)
last_frame(func, -1.5, 1.5, step_size = .002)


x <- seq(-3, 3, by = .002)

#kernel <- unif(x, -.5, .5)
kernel <- absolute(x, 1, -1, 1)
#kernel <- linear(x, -.5, .5, -1, 1)
first_frame(kernel, -3, 3, )
last_frame(kernel, -3, 3, )
generate_mp4(kernel, -3, 3, step_size = .002, freq = 4, fps = 30, nIter = 3)
############################################################################################
