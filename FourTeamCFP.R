### 4 team CFP simulation

# load libraries
library(tidyverse)
library(gt)
library(cfbfastR)
library(cfbplotR)
library(webshot2)

# Define teams and probabilities
teams <- c("Michigan", "Washington", "Texas", "Alabama")
probabilities <- matrix(c(NA, 0.69, 0.60, 0.52, 
                          0.31, NA, 0.40, 0.33, 
                          0.40, 0.60, NA, 0.42, 
                          0.48, 0.67, 0.58, NA), nrow = 4)

# Number of simulations
n_simulations <- 10000

# Simulation code
# Initialize counters
finals_count <- setNames(rep(0, 4), teams)
championships_count <- setNames(rep(0, 4), teams)

# Simulation function
simulate_playoff <- function(teams, probabilities, finals_count, championships_count) {
  # Semifinals
  semi_finals_winners <- c(
    ifelse(runif(1) < probabilities[1, 4], teams[4], teams[1]),  # Team1 vs Team4
    ifelse(runif(1) < probabilities[2, 3], teams[3], teams[2])   # Team2 vs Team3
  )
  
  # Update finals appearances
  finals_count[semi_finals_winners] <- finals_count[semi_finals_winners] + 1
  
  # Finals
  champion_index <- ifelse(runif(1) < probabilities[which(teams == semi_finals_winners[1]), which(teams == semi_finals_winners[2])], 2, 1)
  champion <- semi_finals_winners[champion_index]
  
  # Update championships
  championships_count[champion] <- championships_count[champion] + 1
  
  # Return updated counts
  return(list(finals = finals_count, championships = championships_count))
}

# Run simulations
set.seed(123)
for (i in 1:n_simulations) {
  results <- simulate_playoff(teams, probabilities, finals_count, championships_count)
  finals_count <- results$finals
  championships_count <- results$championships
}

# Create a data frame for results
results_df <- data.frame(
  Team = names(results$finals),
  FinalsAppearances = results$finals,
  Championships = results$championships
)

# Add team logos
results_df$Logo <- results_df$Team

# Create gt table with team logos
results_df %>% arrange(desc(Championships)) %>% 
  mutate(frac_semi = FinalsAppearances / n_simulations) %>%
  mutate(frac_champ = Championships / n_simulations) %>%
  select(-FinalsAppearances, -Championships) %>%
  gt() %>%
  gt_fmt_cfb_logo(columns = "Logo") %>%
  cols_move_to_start(Logo) %>%
  cols_label(Logo = "",
             Team = "Team",
             frac_semi = "Advance to Final",
             frac_champ = "Win Championship") %>%
  cols_align(align = "center", columns = c(frac_semi,frac_champ)) %>%
  tab_options(column_labels.font.weight = "bold") %>%
  fmt_percent(
    columns = c(frac_semi,frac_champ),
    decimals = 1)

