
library(ggplot2)

#' @returns                 # saves a .mp4 file in the
#' @export
#'
#' @examples
#' # Calling the LRMultiClass function on a 10 x 101 Matrix X, with first column all ones,
#' # and a vector of class assignments Y
#'

# Creates a discrete uniform vector on the interval (start, end) with number of values equal to "size"
discrete_vector <- function(start, end, size){

  vec <- rep(0, size)                       # Initialize vector

  step_size <- (end - start + 1)/size       # Calculate the distance between values
  vec[1] <- start                           # First element is "start" value
  vec[size] <- end                          # Last element is "end" value

  for(i in 2:(size-1)){
    vec[i] <- vec[i - 1] + step_size        # Fill in the vector
  }

  return(vec)
}

#generates a probability vector for a discrete uniform vector.
prob_vector <- function(discrete_vec){

  size <- length(discrete_vec)      # Initialize size of the vector

  prob_vec <- rep(1/size, size)     # Discrete uniform distribution has equal entries

  return(prob_vec)
}

# plots the mass function of a random variable with a given outcome vector and probability vector
plot_discrete <- function(discrete_vec, prob_vec){

  # Initialize values to determine labels for X-axis
  maximum = max(discrete_vec)
  minimum = min(discrete_vec)
  num = ceiling(maximum / 10)

  # This n is being computed assuming that the random variable is a die roll
  # This needs to be generalized
  n = discrete_vec[1]
  title_text <- paste0("Sum of ", n, " Independent Die Rolls")

  # Set a value for the upper limit of the Y axis
  upper = 2 * max(prob_vec)

  # Create data fram for plot
  data <- data.frame(x = discrete_vec, y = prob_vec)

  # Generate the plot of the mass function
  plot <- ggplot(data, aes(x = factor(x), y = y)) +  # Convert x to a factor to ensure discrete x-axis
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Bar plot
    scale_x_discrete(breaks = seq(minimum, maximum, by = num)) +
    labs(title = title_text,
         x = "Sum of the Variables", y = "Probability") +
    theme_minimal()

  return(plot)
}

# given two discrete vectors and corresponding probability vectors, computes and returns the codomain of their sum
# and the corresponding probability vector.
discrete_conv <- function(disc_vec_1, prob_vec_1, disc_vec_2, prob_vec_2){

  # Create outcome matrices for the input random variables
  conv_mat1 <- matrix(rep(disc_vec_1, length(disc_vec_2)), nrow = length(disc_vec_1), ncol = length(disc_vec_2))
  conv_mat2 <- matrix(rep(disc_vec_2, length(disc_vec_1)), nrow = length(disc_vec_2), ncol = length(disc_vec_1))

  # Create an outcome matrix for the sum of the two random variable, then flatten the matrix
  conv_mat <- conv_mat1 + t(conv_mat2)
  conv_vec <- as.vector(conv_mat)

  # Calculate a probability matrix for the outcomes, then flatten the matrix
  prob_mat <- as.matrix(prob_vec_1) %*% t(as.matrix(prob_vec_2))
  prob_vec <- as.vector(prob_mat)

  # Condense the outcome vector
  outcomes <- unique(conv_vec)
  probabilities <- rep(0, length(outcomes))

  # Calculate the corresponding probabilites for the condensed outcome vector
  for(i in 1:length(outcomes)){
    probabilities[i] = sum(prob_vec[conv_vec == outcomes[i]])
  }

  # Return a list of the outcomes and their respective probabilities
  return(list(outcomes = outcomes, probabilities = probabilities))
}

# Generates a sequence of plots of the sum of independed discrete uniform random variables
convolution_sequence <- function(disc_vec, disc_prob, seq_len){

  # Ensure proper input
  if(seq_len < 0){
    stop("Positive sequence length required.")
  }

  # Plot the initial mass function
  plot <- plot_discrete(disc_vec, disc_prob)
  print(plot)

  # Initialize the mass function of the sum
  disc_vec_conv <- disc_vec
  disc_prob_conv <- disc_prob

  # In each loop, we compute the mass function for the ith partial sum and then plot the mass function
  for(i in 2:seq_len){

    # Employ conv function to determine mass function of the ith partial sum
    conv <- discrete_conv(disc_vec, disc_prob, disc_vec_conv, disc_prob_conv)

    # Save the mass function for use in the next iteration
    disc_vec_conv <- conv$outcomes
    disc_prob_conv <- conv$probabilities

    # Plot the ith mass function
    plot <- plot_discrete(disc_vec_conv, disc_prob_conv)
    print(plot)

  }

}
