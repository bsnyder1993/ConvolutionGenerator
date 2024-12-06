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


n <- 1
step_size <- .002

plot_files <- character(501*n)

x_vals <- seq(-2, 2, by = step_size)

length(x_vals)


y3 <- rep(0, length(x_vals))
y1 <- unif(x_vals, 1, -.5, .5)
y2 <- y1
y2$vec <- ashift(y2$vec, -1000)
index <- 1

for(i in 1:length(x_vals)){

  center <- -2.5 + step_size * (i - 1)

  #y2 <- unif(x_vals, 1, center - .5, center + .5)
  y2$vec <- ashift(y2$vec, 1)

  #y3[i - .5 / step_size] <- convolution(x, y1$vec, y2$vec, (1/step_size))
  y3[i] <- convolution(x, y1$vec, y2$vec, (1/step_size))
  y4 <- ashift(y3, 500)

  if((i-1) %% 4 == 0){

    data <- data.frame(
      x = rep(x_vals, 3),
      y = c(y1$vec, y2$vec, y4),
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

    plot_file <- paste0("plot_", index, ".png")
    ggsave(plot_file, plot = plot, width = 5, height = 4)

    plot_files[index] <- plot_file

    index <- index + 1

  }

}

framerate <- 30

av_encode_video(plot_files, output = "compound_convolution.mp4", framerate = framerate)

file.remove(plot_files)

############################################################################################

install.packages("magic")

x <- c(1, 2, 3, 4)
x <- ashift(x, -1)
x

