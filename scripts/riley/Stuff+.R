###############################################

## Stuff+
## Will Paz
## 9.16.24

###############################################
# Load nessecary packages
library(tidyverse)
library(xgboost)
library(gridGraphics)
library(vip)
library(kableExtra)

###############################################
# Set seed
set.seed(42069) # heheheha

###############################################

#-------------- PREPROCESSING -----------------

###############################################
#Load in the Data
data2024 <- read_csv("College Data/2024TMDataWhole.csv")
data2023 <- read_csv("College Data/2023_College_Data.csv")
data2022 <- read_csv("College Data/data_2022.csv")
#____Insert linear weights csv____

###############################################

#--------------- Clean 2022 -------------------

# Find Unique Event
unique(data2022$pitchcall)
unique(data2022$playresult)

# Replace specific values in playresult
data2022 <- data2022 %>%
  mutate(playresult = case_when(
    playresult %in% c("Undefined", " ", "") ~ NA,
    playresult == "Fielderschoice" ~ "FieldersChoice",
    playresult == "Homerun" ~ "HomeRun",
    TRUE ~ playresult))

# Replace specific values in pitchcall
data2022 <- data2022 %>%
  mutate(pitchcall = case_when(
    pitchcall %in% c("Undefined", " ", "") ~ NA,
    pitchcall == "BallInDirt" ~ "BallCalled",
    pitchcall == "BallIntentional" ~ "BallCalled",
    pitchcall == "Hitbypitch" ~ "HitByPitch",
    pitchcall == "Inplay" ~ "InPlay",
    pitchcall == "ballCalled" ~ "BallCalled",
    pitchcall == "HitbyPitch" ~ "HitByPitch",
    TRUE ~ pitchcall))

# Replace NAs in playresult with values from pitchcall
data2022 <- data2022 %>%
  mutate(playresult = coalesce(playresult, pitchcall))

# Replace undefined values in taggedpitchtype with autopitchtype
data2022 <- data2022 %>%
  mutate(taggedpitchtype = if_else(taggedpitchtype == "Undefined", autopitchtype, taggedpitchtype))

# Standardize playresult naming and filter out unwanted events
data2022 <- data2022 %>%
  filter(!playresult %in% c(
    "CatchersInterference", "Popup", "FlyBall", "Fastball", "Sinker", "Slider",
    "BattersInterference", "BatterInterference","CatchersInterfernece",
    "Undefined", "Sacrifice", "GroundBall", "Pickoff", "PickOff"
  ))

# Take absolute value of Horizontal Break and Horizontal Release
data2022 <- data2022 %>%
  mutate(
    horzbreak = abs(horzbreak),
    relside = abs(relside),
    Differential_Break = inducedvertbreak - horzbreak
  )

#____Insert the joining of linear weights____

###############################################

#--------------- Clean 2023 -------------------

# Find Unique Event
unique(data2023$pitchcall)
unique(data2023$playresult)

# Replace specific values in playresult
data2023 <- data2023 %>%
  mutate(playresult = case_when(
    playresult %in% c("Undefined", " ", "", ",") ~ NA,
    playresult == "sacrifice" ~ "Sacrifice",
    playresult == "Homerun" ~ "HomeRun",
    playresult == "SIngle" ~ "Single",
    playresult == "OUt" ~ "Out",
    TRUE ~ playresult))

# Replace specific values in pitchcall
data2023 <- data2023 %>%
  mutate(pitchcall = case_when(
    pitchcall %in% c("Undefined", " ", "", ",") ~ NA,
    pitchcall == "BallinDirt" ~ "BallCalled",
    pitchcall == "BallIntentional" ~ "BallCalled",
    pitchcall == "WildPitch" ~ "BallCalled",
    pitchcall == "SwinginStrike" ~ "StrikeSwinging",
    pitchcall == "StriekSwinging" ~ "StrikeSwinging",
    TRUE ~ pitchcall))

# Replace NAs in playresult with values from pitchcall
data2023 <- data2023 %>%
  mutate(playresult = coalesce(playresult, pitchcall))

# Replace undefined values in taggedpitchtype with autopitchtype
data2023 <- data2023 %>%
  mutate(taggedpitchtype = if_else(taggedpitchtype == "Undefined", autopitchtype, taggedpitchtype))

# Filter out unwanted play results
data2023 <- data2023 %>%
  filter(!playresult %in% c(
    "CatchersInterference", "StolenBase", "CaughtStealing", 
    "BattersInterference", "BallIntentional", "Fastball", 
    "CatchersInterfernece", "sacrifice", "Undefined", "InPlay"
  ))

# Take absolute value of Horizontal Break and Horizontal Release
data2023 <- data2023 %>%
  mutate(
    horzbreak = abs(horzbreak),
    relside = abs(relside),
    Differential_Break = inducedvertbreak - horzbreak
  )

#____Insert the joining of linear weights____

###############################################

#--------------- Clean 2024 -------------------

# Find Unique Event
unique(data2024$pitchcall)
unique(data2024$playresult)

# Replace specific values in playresult
data2024 <- data2024 %>%
  mutate(playresult = case_when(
    playresult %in% c("Undefined", " ", "", ",") ~ NA,
    playresult == "SIngle" ~ "Single",
    playresult == "homerun" ~ "HomeRun",
    playresult == "error" ~ "Error",
    playresult == "Homerun" ~ "HomeRun",
    TRUE ~ playresult))

# Replace specific values in pitchcall
data2024 <- data2024 %>%
  mutate(pitchcall = case_when(
    pitchcall %in% c("Undefined", " ", "", ",") ~ NA,
    pitchcall == "BallinDirt" ~ "BallCalled",
    pitchcall == "BallIntentional" ~ "BallCalled",
    pitchcall == "BalIntentional" ~ "BallCalled",
    pitchcall == "FoulBallFieldable" ~ "FoulBall",
    pitchcall == "FoulBallNotFieldable" ~ "FoulBall",
    pitchcall == "StriekC" ~ "StrikeCalled",
    pitchcall == "FouldBallNotFieldable" ~ "FoulBall",
    pitchcall == "SrikeCalled" ~ "StrikeCalled",
    pitchcall == "StrkeSwinging" ~ "StrikeSwinging",
    TRUE ~ pitchcall))

# Replace NAs in playresult with values from pitchcall
data2024 <- data2024 %>%
  mutate(playresult = coalesce(playresult, pitchcall))

# Replace undefined values in taggedpitchtype with autopitchtype
data2024 <- data2024 %>%
  mutate(taggedpitchtype = if_else(taggedpitchtype == "Undefined", autopitchtype, taggedpitchtype))

# Filter out unwanted play results
data2024 <- data2024 %>%
  filter(!playresult %in% c(
    "CatchersInterference", "StolenBase", "CaughtStealing", 
    "BattersInterference", "BallIntentional", "Fastball", 
    "CatchersInterfernece", "sacrifice", "Undefined", "InPlay"
  ))

# Take absolute value of Horizontal Break and Horizontal Release
data2024 <- data2024 %>%
  mutate(
    horzbreak = abs(horzbreak),
    relside = abs(relside),
    Differential_Break = inducedvertbreak - horzbreak
  )

#____Insert the joining of linear weights____

###############################################
















