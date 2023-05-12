# Calculate total ability and rank of 3 players for every possible collaboration

# Load libraries
library(tidyverse)
library(dplyr)

# Generate individual ability
ability_1 <- .2
ability_2 <- .99
ability_3 <- 1
df <- data.frame(
    player = c(1, 2, 3),
    lone_ability = c(ability_1, ability_2, ability_3),
    total_ability = c(NA, NA, NA),
    rank = c(NA, NA, NA),
    p_promotion = c(NA, NA, NA)
    )

max_lone_ability <- max(df$lone_ability)
total_players <- length(unique(df$player))

collaborations <- list(
  # no collab
  data.frame(
    collaborate_1 = c(0, 0, 0),
    collaborate_2 = c(0, 0, 0),
    collaborate_3 = c(0, 0, 0)
  ),
  # 1,2 collab
  data.frame(
    collaborate_1 = c(0, 1, 0),
    collaborate_2 = c(1, 0, 0),
    collaborate_3 = c(0, 0, 0)
  ),
  # 1,3 collab
  data.frame(
    collaborate_1 = c(0, 0, 1),
    collaborate_2 = c(0, 0, 0),
    collaborate_3 = c(1, 0, 0)
  ),
  # 2,3 collab
  data.frame(
    collaborate_1 = c(0, 0, 0),
    collaborate_2 = c(0, 0, 1),
    collaborate_3 = c(0, 1, 0)
  ),
  # 1,2; 1,3 collab
  data.frame(
    collaborate_1 = c(0, 1, 1),
    collaborate_2 = c(1, 0, 0),
    collaborate_3 = c(1, 0, 0)
  ),
  # 1,2; 2,3 collab
  data.frame(
    collaborate_1 = c(0, 1, 0),
    collaborate_2 = c(1, 0, 1),
    collaborate_3 = c(0, 1, 0)
  ),
  # 1,3; 2,3 collab
  data.frame(
    collaborate_1 = c(0, 0, 1),
    collaborate_2 = c(0, 0, 1),
    collaborate_3 = c(1, 1, 0)
  ),
  # 1,2; 1,3; 2,3 collab
  data.frame(
    collaborate_1 = c(0, 1, 1),
    collaborate_2 = c(1, 0, 1),
    collaborate_3 = c(1, 1, 0)
  )
)

# Define function which gives collab ability for each collab combo
get_collab_ability <- function(collab_i) {
  collab_ability <- t(as.matrix(df$lone_ability)) %*%
    as.matrix(collaborations[[collab_i]])
  return(collab_ability)
}

# Define total ability function
total_ability <- function(player_i, which_collab) {
  # sum over collaborator ability here
  collab_ability <- get_collab_ability(which_collab)[player_i]
  total_ability <- 1 / (2 * max_lone_ability) *
    (df$lone_ability[player_i] + 1 / total_players * collab_ability)
return(total_ability)
}

# Define rank function, returns (rank_1, rank_2, rank_3)
get_rank <- function(total_ability_i) {
  rank <- c(NA, NA, NA)
  for (j in 1:3) {
    if (total_ability_i[j] >= max(total_ability_i[-j])) {
      rank[j] <- 1
    } else if (total_ability_i[j] <= min(total_ability_i[-j])) {
      rank[j] <- length(unique(total_ability_i[-j])) + 1
    } else {
      rank[j] <- 2
    }
  }
  return(rank)
}

# For each collab combo, calculate each player's probability of promotion
for (i in seq_along(collaborations)) {
  print(i)
  # Calculate total ability for each player
  df$total_ability <- mapply(total_ability, df$player, i)
  # Calculate rank for each player
  df$rank <- get_rank(df$total_ability)
  # Calculate probability of promotion for each player
  df$p_promotion <- .9 * (total_players - df$rank) / total_players +
    .1 * df$total_ability
  # Print results
  print(df)
}


# Redo this simulation but using the formulation from whiteboard