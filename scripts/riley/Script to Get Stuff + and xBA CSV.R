#Load libraries
library(dplyr)
library(xgboost)
library(ggplot2)
#install.packages("gridGraphics")
library(gridGraphics)
library(vip)
library(kableExtra)
library(knitr)
set.seed(1234)

#Load in the Data
data2023 <- read.csv("2023_College_Data.csv")
data2022 <- read.csv("data_2022.csv")
LinearWeights <- read.csv("MLB_LinearWeight_For_TM.csv")

data2023 <- data2023 %>%
  mutate(pitcherid = ifelse(pitcher == "Pauley, Cole", 10097331, pitcherid))

#Remove game_year
data2022 <- data2022 %>% select(-game_year)

#---------------------Clean 2022 Data-----------------------
#Make Event Blanks NAs
data2022$playresult[data2022$playresult == "Undefined" | data2022$playresult == " " | data2022$playresult == ""] <- NA

#Replace NAs in Events column with values from description column
data2022_new <- data2022
data2022_new$playresult[is.na(data2022_new$playresult)] <- data2022_new$pitchcall[is.na(data2022_new$playresult)]

#Replace undefined values in taggedpitchtype
data2022_new$taggedpitchtype[data2022_new$taggedpitchtype == 'Undefined'] <- data2022_new$autopitchtype[data2022_new$taggedpitchtype == 'Undefined']
#--------------Rename play results to line up with LinearWeights and Filter out results not wanted-------------

unique(data2022_new$playresult)

data2022_new$playresult <- gsub("ballCalled", "BallCalled", data2022_new$playresult)
data2022_new$playresult <- gsub("BallinDirt", "BallCalled", data2022_new$playresult)
data2022_new$playresult <- gsub("BallIntentional", "BallCalled", data2022_new$playresult)
data2022_new$playresult <- gsub("Fielderschoice", "FieldersChoice", data2022_new$playresult)
data2022_new$playresult <- gsub("Hitbypitch", "HitByPitch", data2022_new$playresult)
data2022_new$playresult <- gsub("HitbyPitch", "HitByPitch", data2022_new$playresult)

data2022_new <- filter(data2022_new, 
                       playresult != "CatchersInterference" &
                         playresult != "Popup" &
                         playresult != "FlyBall" &
                         playresult != "BattersInterference" &
                         playresult != "BatterInterference" &
                         playresult != "Slider" &
                         playresult != "Fastball" &
                         playresult != "CatchersInterfernece" &
                         playresult != "Undefined" &
                         playresult != "Sacrifice" &
                         playresult != "Sinker" &
                         playresult != "GroundBall" &
                         playresult != "Pickoff" &
                         playresult != "PickOff")

#Left Join data and weights
data2022_new <- left_join(LinearWeights, data2022_new, by = c("Event" = "playresult"))

#Take abs value of Horizontal Break and Horizontal Release
data2022_new$horzbreak <- abs(data2022_new$horzbreak)

data2022_new$relside <- abs(data2022_new$relside)

#Differential Break
data2022_new$Differential_Break <- data2022_new$inducedvertbreak - data2022_new$horzbreak

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

model_xgb <- readRDS("model_xgb.rds")
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
  group_by(pitcherid, pitcher, taggedpitchtype, pitcherteam) %>%
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

PredictionFinal <- filter(PredictionFinal, Pitches >= 50 | pitcherteam == "MIA_RED" | pitcher == "Olejnik, Peyton" |
                            pitcher == "Harajli, Ahmad" | pitcher == "Galdoni, Lukas" | 
                            pitcher == "Pauley, Cole" | pitcher == "Zimmer, Ryan")

#-----------------Scale xRV/100 to Stuff + Scale----------------
PredictionFinal$`xRV/100ScaledNegative` <- PredictionFinal$`xRV/100` - max(PredictionFinal$`xRV/100`)

PredictionFinal$`ABSxRV/100ScaledNeg` <- abs(PredictionFinal$`xRV/100ScaledNegative`)

PredictionFinal$`Stuff+` <- (PredictionFinal$`ABSxRV/100ScaledNeg` / mean(PredictionFinal$`ABSxRV/100ScaledNeg`)) * 100


#---------------Fastball---------------
FBs4S <- filter(PredictionFinal, taggedpitchtype == "Fastball")

FBs4S$`xRV/100ScaledNegative` <- FBs4S$`xRV/100` - max(FBs4S$`xRV/100`)

FBs4S$`ABSxRV/100ScaledNeg` <- abs(FBs4S$`xRV/100ScaledNegative`)

FBs4S$`Stuff+` <- (FBs4S$`ABSxRV/100ScaledNeg` / mean(FBs4S$`ABSxRV/100ScaledNeg`)) * 100


#--------------Sinkers-------------
SI <- filter(PredictionFinal, taggedpitchtype == "Sinker")

SI$`xRV/100ScaledNegative` <- SI$`xRV/100` - max(SI$`xRV/100`)

SI$`ABSxRV/100ScaledNeg` <- abs(SI$`xRV/100ScaledNegative`)

SI$`Stuff+` <- (SI$`ABSxRV/100ScaledNeg` / mean(SI$`ABSxRV/100ScaledNeg`)) * 100

#---------------Cutters----------------
CT <- filter(PredictionFinal, taggedpitchtype == "Cutter")

CT$`xRV/100ScaledNegative` <- CT$`xRV/100` - max(CT$`xRV/100`)

CT$`ABSxRV/100ScaledNeg` <- abs(CT$`xRV/100ScaledNegative`)

CT$`Stuff+` <- (CT$`ABSxRV/100ScaledNeg` / mean(CT$`ABSxRV/100ScaledNeg`)) * 100

#-----------Sliders-----------
SLIndyScaled <- filter(PredictionFinal, taggedpitchtype == "Slider")

SLIndyScaled $`xRV/100ScaledNegative` <- SLIndyScaled $`xRV/100` - max(SLIndyScaled $`xRV/100`)

SLIndyScaled $`ABSxRV/100ScaledNeg` <- abs(SLIndyScaled $`xRV/100ScaledNegative`)

SLIndyScaled $`Stuff+` <- (SLIndyScaled $`ABSxRV/100ScaledNeg` / mean(SLIndyScaled $`ABSxRV/100ScaledNeg`)) * 100


#-----------Curveballs-----------
CBIndyScaled <- filter(PredictionFinal, taggedpitchtype == "Curveball")

CBIndyScaled $`xRV/100ScaledNegative` <- CBIndyScaled $`xRV/100` - max(CBIndyScaled $`xRV/100`)

CBIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CBIndyScaled $`xRV/100ScaledNegative`)

CBIndyScaled $`Stuff+` <- (CBIndyScaled $`ABSxRV/100ScaledNeg` / mean(CBIndyScaled $`ABSxRV/100ScaledNeg`)) * 100


#------------Changeups-----------
CHIndyScaled <- filter(PredictionFinal, taggedpitchtype == "ChangeUp" | taggedpitchtype == "Splitter")

CHIndyScaled $`xRV/100ScaledNegative` <- CHIndyScaled $`xRV/100` - max(CHIndyScaled $`xRV/100`)

CHIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CHIndyScaled $`xRV/100ScaledNegative`)

CHIndyScaled $`Stuff+` <- (CHIndyScaled $`ABSxRV/100ScaledNeg` / mean(CHIndyScaled $`ABSxRV/100ScaledNeg`)) * 100


AllPitches <- rbind(FBs4S, SI, CT, SLIndyScaled, CBIndyScaled, CHIndyScaled)

#read in xBA csv
xBA <- read.csv("MiamixBA.csv")

xBAandStuffFinal <- left_join(AllPitches, xBA)

xBAandStuffFinal <- filter(xBAandStuffFinal, pitcherteam == "MIA_RED" | pitcher == "Olejnik, Peyton" |
                             pitcher == "Harajli, Ahmad" | pitcher == "Galdoni, Lukas" | 
                             pitcher == "Pauley, Cole" | pitcher == "Zimmer, Ryan")

xBAandStuffFinal <- subset(xBAandStuffFinal, select = c(pitcherid, pitcher, taggedpitchtype, 
                                                        pitcherteam, Pitches, Velocity, Horizontal_Break, 
                                                        Vertical_Break, Extension, Vertical_Release,
                                                        Horizontal_Release, VAA, `Stuff+`, xBA, 
                                                        Pitches_Considered_for_xBA))


xBAandStuffFinal <- xBAandStuffFinal %>%
  arrange(pitcher, pitcherid)

write.csv(xBAandStuffFinal, "MiamixBAandStuff+withAhmad.csv")







#-----------Messing around----------
mast <- data2023 %>%
  group_by(taggedpitchtype, pitcher, pitcherid) %>%
  filter(pitcher == "Mastrian, Patrick") %>%
  summarise(Velocity = mean(relspeed, na.rm = TRUE),
            Horizontal_Break = mean(horzbreak, na.rm = TRUE),
            Vertical_Break = mean(inducedvertbreak, na.rm = TRUE),
            Extension = mean(extension, na.rm = TRUE),
            Vertical_Release = mean(relheight, na.rm = TRUE),
            Horizontal_Release = mean(relside, na.rm = TRUE))
           
write.csv(Maxey, "Maxey.csv")



