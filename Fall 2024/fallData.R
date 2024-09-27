###############################################

## Create Running Fall DF
## Will Paz
## 9.20.24

###############################################

library(tidyverse)
library(pander)
library(knitr)
library(plyr)
library(gridExtra) 
library(readr)
library(lubridate)

fallGames <- c("College Data/20240918-McKieFieldStad-Private-2_unverified.csv",
               "College Data/20240919-McKieFieldStad-Private-1.csv",
               "College Data/20240920-McKieFieldStad-Private-1_unverified.csv",
               "College Data/20240925-McKieFieldStad-Private-4_unverified.csv",
               "College Data/20240926-McKieFieldStad-Private-1_unverified.csv")

# Read and bind the fall games into fallDF
fallDF <- bind_rows(lapply(fallGames, read_csv)) %>%
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
    data$KorBB == "Walk" ~ "Walk",
    data$KorBB == "Strikeout" ~ "Strikeout",
    data$PitchCall == "BallIntentional" & data$KorBB == "Walk" ~ "IntentionalWalk",
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
  
  tiltSummary$AveTilt <- format(strptime(tiltSummary$secTilt, "%H:%M"), "%H:%M")
  return(tiltSummary)
}

###############################################

#------------- Preprocess fall pitching data -------------------

cleanedPitcherFall <- function(data) {
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
  write.csv(cleaned, "cleanedPitcherFall2024.csv", row.names = FALSE)
}

cleanedPitcherFall(fallDF)

###############################################

#------------- Preprocess fall batting data -------------------

cleanedBatterFall <- function(data) {
  total <- data %>%
    clean_pitch_types() %>%
    clean_pitcher_names() %>%
    set_pitch_order() %>%
    format_date() %>%
    generate_counts() %>%
    clean_play_result() %>%
    apply_barrel_calculation()
  
  # Write to CSV
  write.csv(total, "cleanedBatterFall2024.csv", row.names = FALSE)
}

cleanedBatterFall(fallDF)

###############################################

# Rewrite fallDF after binding rows
write_csv(fallDF, "fallDF.csv")
