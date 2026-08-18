###############################################

## Create 2025 Miami Baseball DF
## Will Paz
## 2.19.25

###############################################

# Assumes working directory is r/ (or update this path)
source("season_cleaning.R")

games2025 <- c("2025 Season/20250214-CSUBallparkNielsen-1.csv",
               "2025 Season/20250215-RileyPark-1.csv",
               "2025 Season/20250215-RileyPark-2.csv",
               "2025 Season/20250218-CSUBallparkNielsen-1.csv",
               "2025 Season/20250222-EastTennesseeState-1.csv",
               "2025 Season/20250222-EastTennesseeState-2.csv",
               "2025 Season/20250223-EastTennesseeState-1.csv",
               "2025 Season/20250225-McKieFieldStad-1_unverified.csv")

# Read and bind the fall games into games2025DF
games2025DF <- bind_rows(lapply(games2025, read_csv)) %>%
  mutate(BatterId = as.character(BatterId),
         CatcherId = as.character(CatcherId)) %>%
  filter(PitcherTeam == "MIA_RED" | BatterTeam == "MIA_RED")

###############################################

#------------- Preprocess 2025 data -------------------

# Unlike fallDataCleaning.R, games2025DF includes both Miami's pitching and
# batting appearances, so pitcher/batter reports are filtered separately.
clean_pitcher_data(games2025DF %>% filter(PitcherTeam == "MIA_RED"), "cleanedPitcher2025.csv")
clean_batter_data(games2025DF %>% filter(BatterTeam == "MIA_RED"), "cleanedBatter2025.csv")

###############################################

# Rewrite games2025DF after binding rows
write_csv(games2025DF, "games2025DF.csv")
