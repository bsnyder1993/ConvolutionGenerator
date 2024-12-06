# This file contains the prototype for the mp4 generation as well as testing code
# The relevant code has been properly commented in the mp4_generation_functions.R file
library(ggplot2)
library(magic)
library(magick)
library(av)

unif <- function(x, val, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a)-as.numeric(x > b)
  return(list(vec = y, lower = a, upper = b))
}

convolution <- function(x, y1, y2, width){
  area <- sum(pmin(y1, y2))
  return(area / width)
}

generate_mp4 <- function(kernel = NULL, a = -2, b = 2, step_size = .002, freq = 4, fps = 30, nIter = 1){

  a1 <- 2*a
  b1 <- 2*b

  x_vals <- seq(a1, b1, by = step_size)

  a1 <- x_vals[1]
  b1 <- x_vals[length(x_vals)]

  y3 <- rep(0, length(x_vals))
  y1 <- unif(x_vals, 1, -.5, .5)
  y2 <- y1
  y2$vec <- ashift(y2$vec, -(length(x_vals)-1)/2)
  index <- 1

  a_plot <- (a - a1)/step_size
  b_plot <- (b - a1)/step_size

  start <- (a - a1)/(2*step_size)
  end <- ((b + b1)/2 - a1)/step_size

  plot_files <- character((end-start)/freq*nIter)

  for(i in 1:length(x_vals)){

    y2$vec <- ashift(y2$vec, 1)
    y3[i] <- convolution(x, y1$vec, y2$vec, (1/step_size))

    if((i-1) %% 4 == 0 && i >= start && i <= end){

      x_graph <- x_vals[a_plot:b_plot]
      y1_graph <- y1$vec[a_plot:b_plot]
      y2_graph <- y2$vec[a_plot:b_plot]
      y3_graph <- y3[a_plot:b_plot]

      data <- data.frame(
        x = rep(x_graph, 3),
        y = c(y1_graph, y2_graph, y3_graph),
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

  framerate <- 30

  av_encode_video(plot_files, output = "compound_convolution.mp4", framerate = framerate)

  file.remove(plot_files)
}


generate_mp4()
############################################################################################
