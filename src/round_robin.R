round_robin_pairs <- function(teams) {
  n <- length(teams)
  if (n %% 2 != 0) {
    teams <- c(teams, "BYE")
    n <- length(teams)
  }
  
  rounds <- n - 1
  matches_per_round <- n / 2
  
  pairings <- list()
  
  for (round in 1:rounds) {
    round_pairings <- matrix("", nrow = matches_per_round, ncol = 2)
    for (match in 1:matches_per_round) {
      home <- (round - 1 + match) %% (n - 1) + 1
      away <- (n - 1 + round - match) %% (n - 1) + 1
      
      if (away == home) {
        away <- n
      }
      
      round_pairings[match, 1] <- teams[home]
      round_pairings[match, 2] <- teams[away]
    }
    pairings[[round]] <- round_pairings
  }
  
  return(pairings)
}

# Ejemplo de uso
teams <- c("Abdiel", "Alex (PD)", "Los Konse", "Compa del Mirra", "Kingstone", "Eco Pasto")
pairings <- round_robin_pairs(teams)
for (round in 1:length(pairings)) {
  cat("Round", round, ":\n")
  print(pairings[[round]])
  cat("\n")
}
