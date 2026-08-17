#Load libraries
library(tidyverse)
library(xgboost)
#install.packages("gridGraphics")
library(gridGraphics)
library(vip)
library(kableExtra)
set.seed(1234)

#Load in the Data
data2024 <- read_csv("College Data/2024TMDataWhole.csv")
data2023 <- read_csv("College Data/2023_College_Data.csv")
data2022 <- read_csv("College Data/data_2022.csv")
LinearWeights <- read_csv("MLB_LinearWeight_For_TM.csv")

# Remove game_year column
data2022 <- data2022 %>% 
  select(-game_year)

# Find Unique Event
unique(data2022$pitchcall)
unique(data2022$playresult)

#---------------------Clean 2022 Data-----------------------

# Replace specific values with NA in playresult
data2022 <- data2022 %>%
  mutate(playresult = case_when(
    playresult %in% c("Undefined", " ", "") ~ NA,
    playresult == "Fielderschoice" ~ "FieldersChoice",
    playresult == "Homerun" ~ "HomeRun",
    TRUE ~ playresult),
    pitchcall = ifelse(pitchcall %in% c("Fastball", "Sinker", "Slider"), NA, pitchcall))

# Replace NAs in playresult with values from pitchcall
data2022 <- data2022 %>%
  mutate(playresult = coalesce(playresult, pitchcall))

# Replace undefined values in taggedpitchtype with autopitchtype
data2022 <- data2022 %>%
  mutate(taggedpitchtype = if_else(taggedpitchtype == "Undefined", autopitchtype, taggedpitchtype))

#--------------Rename play results to align with LinearWeights and filter out unwanted results-------------

# Standardize playresult naming and filter out unwanted events
data2022 <- data2022 %>%
  mutate(playresult = str_replace_all(playresult, c(
    "ballCalled" = "BallCalled",
    "BallinDirt" = "BallCalled",
    "BallIntentional" = "BallCalled",
    "Hitbypitch|HitbyPitch" = "HitByPitch"
  ))) %>%
  filter(!playresult %in% c(
    "CatchersInterference", "Popup", "FlyBall", 
    "BattersInterference", "BatterInterference","CatchersInterfernece",
    "Undefined", "Sacrifice", "GroundBall", "Pickoff", "PickOff"
  ))

# Left join LinearWeights and data2022
data2022 <- LinearWeights %>%
  left_join(data2022, by = c("Event" = "playresult"))

# Take absolute value of Horizontal Break and Horizontal Release
data2022 <- data2022 %>%
  mutate(
    horzbreak = abs(horzbreak),
    relside = abs(relside),
    Differential_Break = inducedvertbreak - horzbreak
  )

#---------------------------Clean 2023 Data--------------------------
#Only D1 Data
data2023 <- filter(data2023, level == "D1")

#Make Event Blanks NAs
data2023$playresult[data2023$playresult == "Undefined" | data2023$playresult == " " | data2023$playresult == "" | 
                      data2023$playresult == ","] <- NA

#Replace NAs in Events column with values from description column
data2023_new <- data2023
data2023_new$playresult[is.na(data2023_new$playresult)] <- data2023_new$pitchcall[is.na(data2023_new$playresult)]

#Replace undefined values in taggedpitchtype
data2023_new$taggedpitchtype[data2023_new$taggedpitchtype == 'Undefined'] <- data2023_new$autopitchtype[data2023_new$taggedpitchtype == 'Undefined']

#--------Rename play results to line up with LinearWeights and Filter out results not wanted-------

unique(data2023_new$playresult)

data2023_new$playresult <- gsub("BallinDirt", "BallCalled", data2023_new$playresult)
data2023_new$playresult <- gsub("SIngle", "Single", data2023_new$playresult)
data2023_new$playresult <- gsub("SwinginStrike", "StrikeSwinging", data2023_new$playresult)
data2023_new$playresult <- gsub("StriekSwinging", "StrikeSwinging", data2023_new$playresult)
data2023_new$playresult <- gsub("OUt", "Out", data2023_new$playresult)
data2023_new$playresult <- gsub("Homerun", "HomeRun", data2023_new$playresult)


data2023_new <- filter(data2023_new, 
                       playresult != "CatchersInterference" &
                         playresult != "StolenBase" &
                         playresult != "CaughtStealing" &
                         playresult != "BattersInterference" &
                         playresult != "BallIntentional" &
                         playresult != "Fastball" &
                         playresult != "CatchersInterfernece" &
                         playresult != "sacrifice" &
                         playresult != "Undefined" &
                         playresult != "InPlay")

#Left Join data and weights
data2023_new <- left_join(LinearWeights, data2023_new, by = c("Event" = "playresult"))

#Take abs value of Horizontal Break and Horizontal Release
data2023_new$horzbreak <- abs(data2023_new$horzbreak)

data2023_new$relside <- abs(data2023_new$relside)

#Differential Break
data2023_new$Differential_Break <- data2023_new$inducedvertbreak - data2023_new$horzbreak

#Training Data
training <- data2022_new

training <- subset(training, select = c(Linear_weight, relspeed, inducedvertbreak, horzbreak,
                                        extension, relside, relheight, pitcher, pitcherid,
                                        Differential_Break, taggedpitchtype, pitcherteam,
                                        autopitchtype))

training <- na.omit(training)

x <- training[, c("relspeed", "extension", "relside", "relheight",
                  "horzbreak", "inducedvertbreak", "Differential_Break")]
y <- training$Linear_weight

# Test data
testing <- data2023_new 

testing <- subset(testing, select = c(Linear_weight, relspeed, inducedvertbreak, horzbreak,
                                      extension, relside, relheight, pitcher, pitcherid,
                                      Differential_Break, taggedpitchtype, pitcherteam, 
                                      autopitchtype))
testing <- na.omit(testing)

x_test <- as.matrix(testing[, c("relspeed", "extension", "relside", "relheight",
                                "horzbreak", "inducedvertbreak", "Differential_Break")])

# Define the parameter grid
param_grid <- expand.grid(
  ntree = c(100, 125, 150),  
  depth = c(3, 5, 7),        
  alpha = c(0.01, 0.1, 0.5)  
)

#Have variables store best parameters and correlation
best_params <- NULL
best_correlation <- -Inf

#Run over parameter combinations
for (i in 1:nrow(param_grid)) {
  # Train xgboost model with current parameter combination
  model_xgb <- xgboost(data = as.matrix(x), label = y,
                       nrounds = param_grid$ntree[i],
                       max_depth = param_grid$depth[i],
                       alpha = param_grid$alpha[i],
                       objective = "reg:squarederror")
  
  #Predict on test data
  newdata <- xgb.DMatrix(data = x_test)
  predictions <- predict(model_xgb, newdata)
  
  #Combine predictions and test data
  combined <- cbind(as.data.frame(predictions), testing)
  
  combined <- combined %>%
    mutate(pitcherid = ifelse(pitcher == "Vardavas, Nick", 1000241952, pitcherid))  
#Add pitch_type to the final dataset
combined$taggedpitchtype <- testing$taggedpitchtype

  PredictionFinal <- combined %>%
    group_by(pitcherid, pitcher, autopitchtype, pitcherteam) %>%
    summarise(xRV = sum(predictions, na.rm = TRUE),
              n = n(),
              'xRV/100' = 100 * sum(predictions, na.rm = TRUE) / n,
              Velocity = mean(relspeed, na.rm = TRUE),
              Horizontal_Break = mean(horzbreak, na.rm = TRUE),
              Vertical_Break = mean(inducedvertbreak, na.rm = TRUE),
              Extension = mean(extension, na.rm = TRUE),
              Vertical_Release = mean(relheight, na.rm = TRUE),
              Horizontal_Release = mean(relside, na.rm = TRUE),
              Pitches = n(),
              RV = sum(Linear_weight, na.rm = TRUE),
              .groups = "drop")  
  
  PredictionFinal <- filter(PredictionFinal, Pitches >= 50 | pitcherteam == "MIA_RED")
  
  #Calculate correlation between xRV and RV
  correlation <- cor(PredictionFinal$xRV, PredictionFinal$RV)
  
  #See if current parameter combination gives better correlation
  if (correlation > best_correlation) {
    best_correlation <- correlation
    best_params <- param_grid[i, ]
  }
}

# Print best parameters & correlation
cat("Best ntree:", best_params$ntree, "\n")
cat("Best depth:", best_params$depth, "\n")
cat("Best alpha:", best_params$alpha, "\n")
cat("Best correlation:", best_correlation, "\n")

#Save Model
file_path <- '/Volumes/My Passport/model_xgb.rds'

saveRDS(model_xgb, file = file_path)

unique(PredictionFinal$autopitchtype)
#-----------------Scale xRV/100 to Stuff + Scale----------------
PredictionFinal$`xRV/100ScaledNegative` <- PredictionFinal$`xRV/100` - max(PredictionFinal$`xRV/100`)

PredictionFinal$`ABSxRV/100ScaledNeg` <- abs(PredictionFinal$`xRV/100ScaledNegative`)

PredictionFinal$`Stuff+` <- (PredictionFinal$`ABSxRV/100ScaledNeg` / mean(PredictionFinal$`ABSxRV/100ScaledNeg`)) * 100

#---------------Fastball---------------
FBs4S <- filter(PredictionFinal, autopitchtype == "Four-Seam")

FBs4S <- FBs4S[,-(16:17)]

FBs4S$`xRV/100ScaledNegative` <- FBs4S$`xRV/100` - max(FBs4S$`xRV/100`)

FBs4S$`ABSxRV/100ScaledNeg` <- abs(FBs4S$`xRV/100ScaledNegative`)

FBs4S$`Stuff+` <- (FBs4S$`ABSxRV/100ScaledNeg` / mean(FBs4S$`ABSxRV/100ScaledNeg`)) * 100

#--------------Sinkers-------------
SI <- filter(PredictionFinal, autopitchtype == "Sinker")

SI <- SI[,-(16:17)]

SI$`xRV/100ScaledNegative` <- SI$`xRV/100` - max(SI$`xRV/100`)

SI$`ABSxRV/100ScaledNeg` <- abs(SI$`xRV/100ScaledNegative`)

SI$`Stuff+` <- (SI$`ABSxRV/100ScaledNeg` / mean(SI$`ABSxRV/100ScaledNeg`)) * 100

#---------------Cutters----------------
CT <- filter(PredictionFinal, autopitchtype == "Cutter")

CT <- CT[,-(16:17)]

CT$`xRV/100ScaledNegative` <- CT$`xRV/100` - max(CT$`xRV/100`)

CT$`ABSxRV/100ScaledNeg` <- abs(CT$`xRV/100ScaledNegative`)

CT$`Stuff+` <- (CT$`ABSxRV/100ScaledNeg` / mean(CT$`ABSxRV/100ScaledNeg`)) * 100

#-----------Sliders-----------
SL <- filter(PredictionFinal, autopitchtype == "Slider")

SLIndyScaled <- SL[,-(16:17)]

SLIndyScaled $`xRV/100ScaledNegative` <- SLIndyScaled $`xRV/100` - max(SLIndyScaled $`xRV/100`)

SLIndyScaled $`ABSxRV/100ScaledNeg` <- abs(SLIndyScaled $`xRV/100ScaledNegative`)

SLIndyScaled $`Stuff+` <- (SLIndyScaled $`ABSxRV/100ScaledNeg` / mean(SLIndyScaled $`ABSxRV/100ScaledNeg`)) * 100


#-----------Curveballs-----------
CB <- filter(PredictionFinal, autopitchtype == "Curveball")

CBIndyScaled <- CB[,-(16:18)]

CBIndyScaled $`xRV/100ScaledNegative` <- CBIndyScaled $`xRV/100` - max(CBIndyScaled $`xRV/100`)

CBIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CBIndyScaled $`xRV/100ScaledNegative`)

CBIndyScaled $`Stuff+` <- (CBIndyScaled $`ABSxRV/100ScaledNeg` / mean(CBIndyScaled $`ABSxRV/100ScaledNeg`)) * 100


#------------Changeups-----------
CH <- filter(PredictionFinal, autopitchtype == "Changeup" | autopitchtype == "Splitter")

CHIndyScaled <- CH[,-(16:18)]

CHIndyScaled $`xRV/100ScaledNegative` <- CHIndyScaled $`xRV/100` - max(CHIndyScaled $`xRV/100`)

CHIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CHIndyScaled $`xRV/100ScaledNegative`)

CHIndyScaled $`Stuff+` <- (CHIndyScaled $`ABSxRV/100ScaledNeg` / mean(CHIndyScaled $`ABSxRV/100ScaledNeg`)) * 100



#-----------------Miami Pitcher's Stuff Tables---------------
FBs4SMIA <- filter(FBs4S, pitcherteam == "MIA_RED" & n >= 10)
  FBs4SMIA <- filter(FBs4SMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))
  
SIMIA <- filter(SI, pitcherteam == "MIA_RED" & n >= 10)
  SIMIA <- filter(SIMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))
 
CTMIA <- filter(CT, pitcherteam == "MIA_RED" & n >= 10)
  CTMIA <- filter(CTMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))
   
SLIndyScaledMIA <- filter(SLIndyScaled, pitcherteam == "MIA_RED" & n >= 10)
  SLIndyScaledMIA<- filter(SLIndyScaledMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))

CBIndyScaledMIA <- filter(CBIndyScaled, pitcherteam == "MIA_RED" & n >= 10)
  CBIndyScaledMIA<- filter(CBIndyScaledMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))

CHIndyScaledMIA <- filter(CHIndyScaled, pitcherteam == "MIA_RED" & n >= 10)
  CHIndyScaledMIA<- filter(CHIndyScaledMIA, !(pitcher %in% c("Leach, Hudson", "Egbert, Kenten", "Oliver, Connor")))

#-----------Tables------------
  #-------Fastball Table-------
FBTable <- FBs4SMIA[order(FBs4SMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n", "Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]
FBTable <- FBTable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))
  
# Create the caption
caption <- "Miami Fastball Stuff +"
  
# Make Table
tableFBs <- kable(FBTable, caption = caption,
                  col.names = c("Player Name", "Pitch Type", "Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
kable_styling()
  
print(tableFBs)
  
  

#---------Sinker Table--------
SITable <- SIMIA[order(SIMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]

SITable <- SITable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))

# Create the caption
caption <- "Miami Sinker Stuff +"

# Make Table
tableSIs <- kable(SITable, caption = caption,
                  col.names = c("Player Name", "Pitch Type","Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

print(tableSIs)

#----------Cutter Table--------
CTTable <- CTMIA[order(CTMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]

CTTable <- CTTable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))

# Create the caption
caption <- "Miami Cutter Stuff +"

# Make Table
tableCTs <- kable(CTTable, caption = caption,
                  col.names = c("Player Name", "Pitch Type","Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

print(tableCTs)

#Slider Table
SLTable <- SLIndyScaledMIA[order(SLIndyScaledMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]

SLTable <- SLTable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))

# Create the caption
caption <- "Miami Slider Stuff +"

# Make Table
tableSLs <- kable(SLTable, caption = caption,
                  col.names = c("Player Name", "Pitch Type","Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

print(tableSLs)

#Curveball
CBTable <- CBIndyScaledMIA[order(CBIndyScaledMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]

CBTable <- CBTable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))

# Create the caption
caption <- "Miami Curveball Stuff +"

# Make Table
tableCBs <- kable(CBTable, caption = caption,
                  col.names = c("Player Name", "Pitch Type","Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

print(tableCBs)

#Changeup
CHTable <- CHIndyScaledMIA[order(CHIndyScaledMIA$`Stuff+`, decreasing = TRUE), c("pitcher", "autopitchtype", "n","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Extension", "Vertical_Release", "Horizontal_Release")]

CHTable <- CHTable %>% 
  mutate(across(c(`Stuff+`, Velocity, Vertical_Break, Horizontal_Break, Extension, Vertical_Release, Horizontal_Release), round, 2))

# Create the caption
caption <- "Miami Changeup Stuff +"

# Make Table
tableCHs <- kable(CHTable, caption = caption,
                  col.names = c("Player Name", "Pitch Type","Pitches", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

print(tableCHs)




