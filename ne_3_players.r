# Calculate total ability and rank of 3 players for every possible collaboration

# Load libraries
library(tidyverse)

# Generate individual ability
n_1 <- 1 / 3
n_2 <- 2 / 3
n_3 <- 1
max_ability <- max(n_1, n_2, n_3)
n_total <- 3

# Define total ability function
total_ability <- function(n_i, n_j) {
  # n_i is the ability of the player
  # n_j is vector of abilities of n_i's collaborators
  total_ability <- 1 / (2 * max_ability) * (n_i + 1 / n_total * sum(n_j))
return(total_ability)
}

# Generate all possible collaborations
