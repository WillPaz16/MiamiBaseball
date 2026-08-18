###############################################

## Shared Trackman Cleaning Functions
## Used by fallDataCleaning.R and games2025.R so the cleaning logic
## (barrel calc, PlayResult tagging, tilt averaging, etc.) exists once.

###############################################

library(tidyverse)
library(pander)
library(knitr)
library(plyr)
library(gridExtra)
library(readr)
library(lubridate)

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

#------------- Season drivers -------------------

# Cleans an already pitcher-team-filtered data frame into a pitcher CSV
# (includes tilt averaging, which only makes sense from the pitcher's perspective).
clean_pitcher_data <- function(data, output_csv) {
  total <- data %>%
    clean_pitch_types() %>%
    clean_pitcher_names() %>%
    set_pitch_order() %>%
    format_date() %>%
    generate_counts() %>%
    clean_play_result() %>%
    apply_barrel_calculation() %>%
    add_hard_hit()

  tiltData <- clean_tilt_data(total)
  tiltSummary <- calculate_avg_tilt(tiltData)
  cleaned <- merge(total, tiltSummary, by = c("Pitcher", "TaggedPitchType"), all = TRUE)

  write.csv(cleaned, output_csv, row.names = FALSE)
  invisible(cleaned)
}

# Cleans an already batter-team-filtered data frame into a batter CSV
clean_batter_data <- function(data, output_csv) {
  total <- data %>%
    clean_pitch_types() %>%
    clean_pitcher_names() %>%
    set_pitch_order() %>%
    format_date() %>%
    generate_counts() %>%
    clean_play_result() %>%
    apply_barrel_calculation()

  write.csv(total, output_csv, row.names = FALSE)
  invisible(total)
}
