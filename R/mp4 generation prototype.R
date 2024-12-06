# This file contains the prototype for the mp4 generation as well as testing code
# The relevant code has been properly commented in the mp4_generation_functions.R file
library(ggplot2)
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

plot_files <- character(501*3)

x_vals <- seq(-2, 2, by = 0.002)
y3 <- rep(0, length(x_vals))
y1 <- unif(x_vals, 1, -.5, .5)

for(i in 1:501){

  center <- -2.5 + .01 * (i - 1)


  y2 <- unif(x_vals, 1, center - .5, center + .5)


  if(1 > 3){}
  else{

    index <- 5*(i-1) + 1 - 250
    index <- min(index, 2001)

    y3[index] <- convolution(x, y1$vec, y2$vec)
    if(i > 2){
      y3[index - 4] = y3[index - 5] + (y3[index] - y3[index - 5])*1/5
      y3[index - 3] = y3[index - 5] + (y3[index] - y3[index - 5])*2/5
      y3[index - 2] = y3[index - 5] + (y3[index] - y3[index - 5])*3/5
      y3[index - 1] = y3[index - 5] + (y3[index] - y3[index - 5])*4/5
    }

    data <- data.frame(
      x = rep(x_vals, 3),
      y = c(y1$vec, y2$vec, y3),
      function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_vals))
    )

    plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
      geom_line(size = 1) +
      labs(title = "Compund Convolution of Uniform Distributions", x = "X", y = "Density") +
      scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white")
      ) +
      ylim(0,2)
  }

  plot_file <- paste0("plot_", i, ".png")
  ggsave(plot_file, plot = plot, width = 5, height = 4)

  plot_files[i] <- plot_file

}

#framerate <- 25

#av_encode_video(plot_files, output = "conv_vid.mp4", framerate = framerate)

#file.remove(plot_files)

#plot_files <- character(351)

y1$vec <- y3

x_vals <- seq(-2, 2, by = 0.002)
y3 <- rep(0, length(x_vals))

for(i in 1:501){

  center <- -2.5 + .01 * (i - 1)

  #y1 <- unif(x_vals, 1, -.5, .5)
  y2 <- unif(x_vals, 1, center - .5, center + .5)


  if(1 > 3){}
  else{
    index <- 5*(i-1) + 1 - 250
    index <- min(index, 2001)

    y3[index] <- convolution(x, y1$vec, y2$vec)
    if(i > 2){
      y3[index - 4] = y3[index - 5] + (y3[index] - y3[index - 5])*1/5
      y3[index - 3] = y3[index - 5] + (y3[index] - y3[index - 5])*2/5
      y3[index - 2] = y3[index - 5] + (y3[index] - y3[index - 5])*3/5
      y3[index - 1] = y3[index - 5] + (y3[index] - y3[index - 5])*4/5
    }

    data <- data.frame(
      x = rep(x_vals, 3),
      y = c(y1$vec, y2$vec, y3),
      function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_vals))
    )

    plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
      geom_line(size = 1) +
      labs(title = "Compund Convolution of Uniform Distributions", x = "X", y = "Density") +
      scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white")
      ) +
      ylim(0,2)
  }

  plot_file <- paste0("plot_", i + 501, ".png")
  ggsave(plot_file, plot = plot, width = 5, height = 4)

  plot_files[i + 501] <- plot_file

}

y1$vec <- y3

x_vals <- seq(-2, 2, by = 0.002)
y3 <- rep(0, length(x_vals))

for(i in 1:501){

  center <- -2.5 + .01 * (i - 1)

  #y1 <- unif(x_vals, 1, -.5, .5)
  y2 <- unif(x_vals, 1, center - .5, center + .5)


  if(1 > 3){}
  else{
    index <- 5*(i-1) + 1 - 250
    index <- min(index, 2001)

    y3[index] <- convolution(x, y1$vec, y2$vec)
    if(i > 2){
      y3[index - 4] = y3[index - 5] + (y3[index] - y3[index - 5])*1/5
      y3[index - 3] = y3[index - 5] + (y3[index] - y3[index - 5])*2/5
      y3[index - 2] = y3[index - 5] + (y3[index] - y3[index - 5])*3/5
      y3[index - 1] = y3[index - 5] + (y3[index] - y3[index - 5])*4/5
    }

    data <- data.frame(
      x = rep(x_vals, 3),
      y = c(y1$vec, y2$vec, y3),
      function_type = rep(c("Function", "Kernel", "Convolution"), each = length(x_vals))
    )

    plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
      geom_line(size = 1) +
      labs(title = "Compund Convolution of Uniform Distributions", x = "X", y = "Density") +
      scale_color_manual(values = c("black", "blue", "red")) +  # Set custom colors
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white")
      ) +
      ylim(0,2)
  }

  plot_file <- paste0("plot_", i + 2 * 501, ".png")
  ggsave(plot_file, plot = plot, width = 5, height = 4)

  plot_files[i + 2 * 501] <- plot_file

}

framerate <- 30

av_encode_video(plot_files, output = "compound_convolution.mp4", framerate = framerate)

file.remove(plot_files)
