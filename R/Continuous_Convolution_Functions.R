
library(ggplot2)

unif <- function(x, val, a, b){
  y <- rep(0, length(x))
  y <- y + as.numeric(x > a)-as.numeric(x > b)
}

convolution <- function(x, y1, y2){
  length <- sum(as.numeric(y1 == y2 & y1 == 1))
  return(length / 100)
}

x_vals <- seq(-2, 2, by = 0.01)
y3 <- rep(0, length(x_vals))

for(i in 1:50){

  center <- -1.5 + .03 * (i - 1)

  y1 <- unif(x_vals, 1, -.5, .5)
  y2 <- unif(x_vals, 1, center - .5, center + .5)

  if(sum(as.numeric(y1 == y2)) == 0){
    print(sum(as.numeric(y1 == y2)))
    data <- data.frame(
      x = rep(x_vals, 2),
      y = c(y1, y2),
      function_type = rep(c("Blah", "Kernel"), each = length(x_vals))
    )
    plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
      geom_line(size = 1) +
      labs(title = "Is this actually doing anything?", x = "X", y = "Density") +
      scale_color_manual(values = c("blue", "red")) +
      theme_minimal() +
      theme(legend.title = element_blank())+
      ylim(0,2)
  }

  else{
    index <- 3*(i-1) + 1 + 50
    y3[index] <- convolution(x, y1, y2)
    if(i > 2){
      y3[index - 2] = y3[index - 3] + (y3[index] - y3[index - 3])*1/3
      y3[index - 1] = y3[index - 3] + (y3[index] - y3[index - 3])*2/3
    }
    data <- data.frame(
      x = rep(x_vals, 3),
      y = c(y1, y2, y3),
      function_type = rep(c("Blah", "Kernel", "Wowwee"), each = length(x_vals))
    )

    plot <- ggplot(data, aes(x = x, y = y, color = function_type)) +
      geom_line(size = 1) +
      labs(title = "Lets go boys", x = "X", y = "Density") +
      scale_color_manual(values = c("blue", "red", "black")) +  # Set custom colors
      theme_minimal() +
      theme(legend.title = element_blank())+
      ylim(0,2)
  }

  print(plot)
}
