###############################################

## Create 2025 Miami Baseball DF
## Will Paz
## 2.19.25

###############################################

library(tidyverse)
library(pander)
library(knitr)
library(plyr)
library(gridExtra) 
library(readr)
library(lubridate)

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

#------------- Common Utility Functions -------------------

# Function to remove unnecessary pitch types
clean_pitch_types <- function(data) {
  data %>% 
    subset(TaggedPitchType != "Other" & TaggedPitchType != "Knuckleball")
}

# Function to format Pitcher names
clean_pitcher_names <- function(data) {
  data$Pitcher <- sub("(\\w+),\\s(\\w+)", "\\2 \\1", data$Pitcher)
  return(data)
}

# Function to set pitch type order
set_pitch_order <- function(data) {
  data$TaggedPitchType <- factor(data$TaggedPitchType, 
                                 levels = c("Fastball", "Sinker", "Cutter", "Curveball", 
                                            "Slider", "Sweeper", "ChangeUp", "Splitter"))
  return(data)
}

# Function to format dates
format_date <- function(data) {
  data$Date <- as.Date(data$Date, "%m/%d/%Y")
  return(data)
}

# Function to generate counts of Balls-Strikes
generate_counts <- function(data) {
  data$Counts <- str_c(data$Balls, "-", data$Strikes)
  return(data)
}

# Barrel calculation logic
is_barrel <- function(exit_velo, launch_angle) {
  if (is.na(exit_velo) | is.na(launch_angle) | exit_velo < 95 | !between(launch_angle, 8, 50)) {
    return(FALSE)
  } 
  exit_velo_ranges <- seq(95, 110, by = 1)
  angle_ranges <- list(c(26, 30), c(25, 31), c(24, 33), c(23, 34), c(21, 35), c(20, 36), 
                       c(19, 38), c(18, 39), c(16, 40), c(15, 41), c(14, 43), c(13, 44), 
                       c(11, 45), c(10, 46), c(9, 48), c(8, 50))
  
  for (i in seq_along(exit_velo_ranges)) {
    if (exit_velo >= exit_velo_ranges[i] & between(launch_angle, angle_ranges[[i]][1], angle_ranges[[i]][2])) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

add_hard_hit <- function(data) {
  data <- data %>%
    mutate(HardHit = ifelse(ExitSpeed >= 95, TRUE, FALSE))
  return(data)
}


# Function to apply barrel calculation (vectorized)
apply_barrel_calculation <- function(data) {
  data$Barrel <- mapply(is_barrel, data$ExitSpeed, data$Angle)
  return(data)
}

# Function to clean PlayResult
clean_play_result <- function(data) {
  data$PlayResult <- case_when(
    data$PitchCall == "BallIntentional" & data$KorBB == "Walk" ~ "IntentionalWalk",
    data$KorBB == "Walk" ~ "Walk",
    data$KorBB == "Strikeout" ~ "Strikeout",
    data$PitchCall == "HitByPitch" ~ "HitByPitch",
    TRUE ~ data$PlayResult
  )
  
  # Set exit speed and angle to NA for HitByPitch
  data <- data %>% 
    mutate(ExitSpeed = ifelse(PitchCall == "HitByPitch", NA, ExitSpeed),
           Angle = ifelse(PitchCall == "HitByPitch", NA, Angle))
  return(data)
}

# Function to convert tilt time to seconds
convert_to_seconds <- function(hand, time) {
  time_parts <- strsplit(time, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  seconds <- as.numeric(time_parts[3])
  
  # Adjust hours based on handedness
  hours <- ifelse(hand == "Right" & hours == 12, 0, hours)
  hours <- ifelse(hand == "Right" & hours == 11, -1, hours)
  hours <- ifelse(hand == "Left" & hours == 1, 13, hours)
  
  return(hours * 3600 + minutes * 60 + seconds)
}

# Function to clean and process tilt data
clean_tilt_data <- function(data) {
  data <- data %>%
    filter(Tilt != '') %>%
    mutate(Tilt = paste0(Tilt, ":00"),
           secTilt = mapply(convert_to_seconds, PitcherThrows, Tilt))
  return(data)
}

# Function to calculate average tilt
calculate_avg_tilt <- function(data) {
  tiltSummary <- aggregate(data[, "secTilt"], 
                           list(Pitcher = data$Pitcher, TaggedPitchType = data$TaggedPitchType), 
                           mean, na.rm = TRUE)
  
  seconds <- tiltSummary$secTilt
  hours <- as.integer(floor(seconds / 3600)) %% 24
  minutes <- as.integer(floor(seconds %% 3600 / 60))
  hours <- ifelse(hours == 0, 12, hours)
  tiltSummary$AveTilt <- sprintf("%d:%02d", hours, minutes)
  return(tiltSummary)
}

###############################################

#------------- Preprocess fall pitching data -------------------

cleanedPitcher2025 <- function(data) {
  data <- data %>% 
    filter(PitcherTeam == "MIA_RED")
  
  total <- data %>%
    clean_pitch_types() %>%
    clean_pitcher_names() %>%
    set_pitch_order() %>%
    format_date() %>%
    generate_counts() %>%
    clean_play_result() %>%
    apply_barrel_calculation() %>% 
    add_hard_hit()
  
  # Process tilt data
  tiltData <- clean_tilt_data(total)
  tiltSummary <- calculate_avg_tilt(tiltData)
  
  # Merge tilt summary
  cleaned <- merge(total, tiltSummary, by = c("Pitcher", "TaggedPitchType"), all = TRUE)
  
  # Write to CSV
  write.csv(cleaned, "cleanedPitcher2025.csv", row.names = FALSE)
}

cleanedPitcher2025(games2025DF)

###############################################

#------------- Preprocess fall batting data -------------------

cleanedBatter2025 <- function(data) {
  data <- data %>% 
    filter(BatterTeam == "MIA_RED")
  
  total <- data %>%
    clean_pitch_types() %>%
    clean_pitcher_names() %>%
    set_pitch_order() %>%
    format_date() %>%
    generate_counts() %>%
    clean_play_result() %>%
    apply_barrel_calculation()
  
  # Write to CSV
  write.csv(total, "cleanedBatter2025.csv", row.names = FALSE)
}

cleanedBatter2025(games2025DF)

###############################################

# Rewrite games2025DF after binding rows
write_csv(games2025DF, "games2025DF.csv")
