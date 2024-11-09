library(ggplot2)

discrete_vector <- function(start, end, size){
  vec <- rep(0, size)

  step_size <- (end - start + 1)/size
  vec[1] <- start
  vec[size] <- end

  for(i in 2:(size-1)){
    vec[i] <- vec[i - 1] + step_size
  }

  return(vec)
}

prob_vector <- function(discrete_vec){
  size <- length(discrete_vec)
  prob_vec <- rep(1/size, size)
  return(prob_vec)
}

plot_discrete <- function(discrete_vec, prob_vec){

  upper = 2 * max(prob_vec)
  data <- data.frame(x = discrete_vec, y = prob_vec)

  plot <- ggplot(data, aes(x = factor(x), y = y)) +  # Convert x to a factor to ensure discrete x-axis
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Bar plot
    labs(title = "Bar Plot of Discrete Values and Probabilities",
         x = "Discrete Values", y = "Probabilities") +
    theme_minimal()

  return(plot)
}

discrete_conv <- function(disc_vec_1, prob_vec_1, disc_vec_2, prob_vec_2){

  conv_mat1 <- matrix(rep(disc_vec_1, length(disc_vec_2)), nrow = length(disc_vec_1), ncol = length(disc_vec_2))
  conv_mat2 <- matrix(rep(disc_vec_2, length(disc_vec_1)), nrow = length(disc_vec_2), ncol = length(disc_vec_1))

  conv_mat <- conv_mat1 + t(conv_mat2)
  conv_vec <- as.vector(conv_mat)

  prob_mat <- as.matrix(prob_vec_1) %*% t(as.matrix(prob_vec_2))
  prob_vec <- as.vector(prob_mat)

  outcomes <- unique(conv_vec)
  probabilities <- rep(0, length(outcomes))

  for(i in 1:length(outcomes)){
    probabilities[i] = sum(prob_vec[conv_vec == outcomes[i]])
  }

  return(list(outcomes = outcomes, probabilities = probabilities))
}

convolution_sequence <- function(disc_vec, disc_prob, seq_len){

  if(seq_len < 0){
    stop("Positive sequence length required.")
  }

  plot <- plot_discrete(disc_vec, disc_prob)

  print(plot)

  disc_vec_conv <- disc_vec
  disc_prob_conv <- disc_prob


  for(i in 2:seq_len){

    conv <- discrete_conv(disc_vec, disc_prob, disc_vec_conv, disc_prob_conv)

    disc_vec_conv <- conv$outcomes
    disc_prob_conv <- conv$probabilities

    plot <- plot_discrete(disc_vec_conv, disc_prob_conv)
    print(plot)

  }

}



# Testing



x1 <- discrete_vector(1, 6, 6)
y1 <- prob_vector(x1)
convolution_sequence(x1, y1, 50)

x2 <- discrete_vector(1, 6, 6)
y2 <- prob_vector(x2)

conv <- discrete_conv(x1, y1, x2, y2)
conv

conv2 <- discrete_conv(x1, y1, conv$outcomes, conv$probabilities)
conv3 <- discrete_conv(x1, y1, conv2$outcomes, conv2$probabilities)

plot <- plot_discrete(conv$outcomes, conv$probabilities)
plot
