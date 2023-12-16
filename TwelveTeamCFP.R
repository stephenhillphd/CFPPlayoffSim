### 12 team CFP simulation

# load libraries
library(tidyverse)
library(gt)
library(cfbfastR)
library(cfbplotR)
library(webshot2)

# Define teams and seedings
teams <- c("Michigan", "Washington", "Texas", "Alabama", "Florida State", "Georgia", "Ohio State", "Oregon", "Missouri", "Penn State", "Ole Miss", "Liberty")

# Define win probabilities matrix
# Format: probabilities[team_row, team_column] is the probability of team_row beating team_column
# Probabilities from Massey Rating match-up tool
# https://masseyratings.com/game.php?s=cf2023
probabilities <- matrix(c(NA,	0.69,	0.6,	0.52,	0.74,	0.51,	0.5,	0.59,	0.82,	0.59,	0.78,	0.91,
                          0.31,	NA,	0.4,	0.33,	0.52,	0.33,	0.31,	0.41,	0.62,	0.38,	0.59,	0.78,
                          0.4,	0.6,	NA,	0.42,	0.64,	0.42,	0.4,	0.5,	0.74,	0.49,	0.7,	0.87,
                          0.48,	0.67,	0.58,	NA,	0.71,	0.5,	0.49,	0.57,	0.8,	0.57,	0.76,	0.9,
                          0.26,	0.48,	0.36,	0.29,	NA,	0.28,	0.25,	0.37,	0.63,	0.34,	0.58,	0.63,
                          0.49,	0.67,	0.58,	0.5,	0.72,	NA,	0.49,	0.58,	0.8,	0.58,	0.8,	0.9,
                          0.5,	0.69,	0.6,	0.51,	0.75,	0.51,	NA,	0.59,	0.83,	0.65,	0.79,	0.92,
                          0.41,	0.59,	0.5,	0.43,	0.63,	0.42,	0.41,	NA,	0.76,	0.49,	0.68,	0.85,
                          0.18,	0.38,	0.26,	0.2,	0.37,	0.2,	0.17,	0.24,	NA,	0.24,	0.46,	0.71,
                          0.41,	0.62,	0.51,	0.43,	0.66,	0.42,	0.35,	0.51,	0.76,	NA,	0.72,	0.88,
                          0.22,	0.41,	0.3,	0.24,	0.42,	0.2,	0.21,	0.32,	0.54,	0.28,	NA,	0.73,
                          0.09,	0.22,	0.13,	0.1,	0.37,	0.1,	0.08,	0.15,	0.29,	0.12,	0.27,	NA),nrow=12)
# Number of simulations
n_simulations <- 10000

# Initialize appearance counters
quarterfinal_appearances <- setNames(rep(0, 12), teams)
semifinal_appearances <- setNames(rep(0, 12), teams)
finals_appearances <- setNames(rep(0, 12), teams)
championships <- setNames(rep(0, 12), teams)

# Initialize a structure to record finals matchups
finals_matchups <- vector("list", n_simulations)

# Simulation function
simulate_playoff <- function(teams, probabilities, quarterfinal_appearances, semifinal_appearances, finals_appearances, championships) {
  # Initial round matchups (5 vs 12, 6 vs 11, 7 vs 10, 8 vs 9)
  initial_matchups <- list(c(5, 12), c(6, 11), c(7, 10), c(8, 9))
  winners_initial <- c()
  
  for (matchup in initial_matchups) {
    #a = runif(1)
    #print(a)
    winner_seed <- ifelse(runif(1) < probabilities[matchup[1], matchup[2]], matchup[2], matchup[1])
    #print(probabilities[matchup[1], matchup[2]])
    winners_initial <- c(winners_initial, winner_seed)
  }
  
  # Quarterfinal matchups
  quarterfinal_matchups <- list(c(4, winners_initial[1]), c(3, winners_initial[2]), c(2, winners_initial[3]), c(1, winners_initial[4]))
  winners_quarterfinal <- c()
  
  for (matchup in quarterfinal_matchups) {
    winner_seed <- ifelse(runif(1) < probabilities[matchup[1], matchup[2]], matchup[2], matchup[1])
    winners_quarterfinal <- c(winners_quarterfinal, winner_seed)
    # Update quarterfinal appearances
    quarterfinal_appearances[teams[matchup]] <- quarterfinal_appearances[teams[matchup]] + 1
  }
  
  # Semifinal matchups
  semifinal_matchups <- list(c(winners_quarterfinal[4], winners_quarterfinal[1]), c(winners_quarterfinal[3], winners_quarterfinal[2]))
  
  winners_semifinal <- c()
  
  for (matchup in semifinal_matchups) {
    winner_seed <- ifelse(runif(1) < probabilities[matchup[1], matchup[2]], matchup[2], matchup[1])
    winners_semifinal <- c(winners_semifinal, winner_seed)
    # Update semifinal appearances
    semifinal_appearances[teams[matchup]] <- semifinal_appearances[teams[matchup]] + 1
    
  }
  
  # Final matchup
  champion_seed <- ifelse(runif(1) < probabilities[winners_semifinal[1], winners_semifinal[2]], winners_semifinal[1], winners_semifinal[2])
  
  # Record the finals matchup
  finals_matchup <- paste(teams[winners_semifinal[1]], "vs", teams[winners_semifinal[2]])
  
  # Update finals appearances
  finals_appearances[teams[winners_semifinal]] <- finals_appearances[teams[winners_semifinal]] + 1
  # Update championships
  championships[teams[champion_seed]] <- championships[teams[champion_seed]] + 1
  
  return(list(quarterfinals = quarterfinal_appearances, semifinals = semifinal_appearances, finals = finals_appearances, championships = championships, finalsMatchup = finals_matchup))
}

# Run simulations
set.seed(123)
for (i in 1:n_simulations) {
  results <- simulate_playoff(teams, probabilities, quarterfinal_appearances, semifinal_appearances, finals_appearances, championships)
  quarterfinal_appearances <- results$quarterfinals
  semifinal_appearances <- results$semifinals
  finals_appearances <- results$finals
  championships <- results$championships
  finals_matchups[[i]] <- results$finalsMatchup
}

# Summarize finals matchups
finals_matchups_summary <- table(unlist(finals_matchups))

# Create a data frame for the summary
finals_matchups_df <- as.data.frame(finals_matchups_summary)
colnames(finals_matchups_df) <- c("FinalsMatchup", "Count")

# Create a data frame for results
results_df <- data.frame(
  Team = teams,
  QuarterfinalAppearances = quarterfinal_appearances,
  SemifinalAppearances = semifinal_appearances,
  FinalsAppearances = finals_appearances,
  Championships = championships
)

# Add team logos
results_df$Logo <- results_df$Team

# Create gt table with team logos
results_df %>% arrange(desc(Championships)) %>%
  mutate(frac_qtr = QuarterfinalAppearances / n_simulations) %>%
  mutate(frac_semi = SemifinalAppearances / n_simulations) %>%
  mutate(frac_finals = FinalsAppearances / n_simulations) %>% 
  mutate(frac_champ = Championships / n_simulations) %>%
  select(-QuarterfinalAppearances, -SemifinalAppearances, -FinalsAppearances, -Championships) %>%
  gt() %>%
  gt_fmt_cfb_logo(columns = "Logo") %>%
  cols_move_to_start(Logo) %>%
  cols_label(Logo = "",
             Team = "Team",
             frac_qtr = "Advance to Quarterfinals",
             frac_semi = "Advance to Semifinals",
             frac_finals = "Advance to Final",
             frac_champ = "Win Championship") %>%
  cols_align(align = "center", columns = c(frac_qtr, frac_semi, frac_finals, frac_champ)) %>%
  tab_options(column_labels.font.weight = "bold") %>%
  fmt_percent(
    columns = c(frac_qtr, frac_semi, frac_finals, frac_champ),
    decimals = 1)

# finals matchups
finals_matchups_df %>% arrange(desc(Count)) %>% gt() %>%
  tab_options(column_labels.font.weight = "bold") %>% 
  cols_label(FinalsMatchup = "Finals Matchup",
             Count = "Count") 