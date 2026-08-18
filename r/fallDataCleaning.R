###############################################

## Create Running Fall DF
## Will Paz
## 9.20.24

###############################################

# Assumes working directory is r/ (or update this path)
source("season_cleaning.R")

BWGame <- read_csv("College Data/20241005-McKieFieldStad-Private-1_unverified.csv")

fallGames <- c("College Data/20240918-McKieFieldStad-Private-2_unverified.csv",
               "College Data/20240919-McKieFieldStad-Private-1.csv",
               "College Data/20240920-McKieFieldStad-Private-1_unverified.csv",
               "College Data/20240925-McKieFieldStad-Private-4_unverified.csv",
               "College Data/20240926-McKieFieldStad-Private-1_unverified.csv",
               "College Data/20241001-McKieFieldStad-Private-1_unverified.csv",
               "College Data/20241003-McKieFieldStad-Private-1_unverified.csv")

# Read and bind the fall games into fallDF
fallDF <- bind_rows(lapply(fallGames, read_csv)) %>%
  mutate(BatterId = as.character(BatterId),
         CatcherId = as.character(CatcherId)) %>%
  bind_rows(BWGame) %>%
  filter(PitcherTeam == "MIA_RED") %>%
  mutate(
    TaggedPitchType = case_when(
      Pitcher == "Berggren, Austin" & TaggedPitchType == "Splitter" ~ "ChangeUp",
      Pitcher == "Byers, Carson" & TaggedPitchType == "Slider" ~ "Curveball",
      Pitcher == "Preisel, Connor" & TaggedPitchType == "Slider" ~ "Curveball",
      Pitcher == "Cuthbertson, Hayden" & TaggedPitchType == "Sinker" ~ "Fastball",
      TRUE ~ TaggedPitchType
    ),
    Pitcher = case_when(
      Pitcher == "Mazey, Chase" & Date == as.Date("2024-09-18") ~ "Colegate, Ryan",
      Pitcher == "Cuthbertson, Hayden" & Inning == 5 & Date == as.Date("2024-09-19") ~ "Landen, Looper",
      Pitcher == "Looper, Landon" ~ "Looper, Landen",
      TRUE ~ Pitcher
    )
  )

###############################################

#------------- Preprocess fall data -------------------

# fallDF is already filtered to PitcherTeam == "MIA_RED" above, so both the
# pitcher and batter reports here are from Miami's pitching appearances
# (i.e. the "batter" report covers opposing hitters faced, not Miami's own
# at-bats) — this matches the original 2024 fall cleaning behavior.
clean_pitcher_data(fallDF, "cleanedPitcherFall2024.csv")
clean_batter_data(fallDF, "cleanedBatterFall2024.csv")

###############################################

# Rewrite fallDF after binding rows
write_csv(fallDF, "fallDF.csv")
