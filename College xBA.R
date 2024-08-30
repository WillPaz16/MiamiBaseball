library(dplyr)
library(randomForest)

data2023 <- read.csv("2023_College_Data.csv")
data2022 <- read.csv("data_2022.csv")

#Change Nick Vardavas and Cole Pauley pitcher ID
data2023 <- data2023 %>%
  mutate(pitcherid = ifelse(pitcher == "Vardavas, Nick", 1000241952, pitcherid))

data2023 <- data2023 %>%
  mutate(pitcherid = ifelse(pitcher == "Pauley, Cole", 10097331, pitcherid))


#Remove game_year
data2022 <- data2022 %>% select(-game_year)

#----------2022 Data---------

#Only Keep Columns we need
data2022_for_xba <- data2022[, c("pitcher", "pitcherid", "taggedpitchtype", "pitchcall",
                                 "playresult", "exitspeed", "angle", "platelocheight", 
                                 "platelocside", "strikes", "pitcherteam")]

unique(data2022_for_xba$playresult)

data2022xBABIP <- filter(data2022_for_xba, pitchcall == "InPlay")

data2022xBABIP$hit <- ifelse(data2022xBABIP$playresult == "HomeRun" | data2022xBABIP$playresult == "Triple" |
                               data2022xBABIP$playresult == "Double" | data2022xBABIP$playresult == "Single", 1, 0)

data2022xBABIP <- data2022xBABIP %>%
  filter(!is.na(hit),
         !is.na(exitspeed),
         !is.na(angle))

#---------2023 Data---------

#Only Keep Columns we need
data2023_for_xba <- data2023[, c("pitcher", "pitcherid", "taggedpitchtype", "pitchcall",
                                 "playresult", "exitspeed", "angle", "platelocheight", 
                                 "platelocside", "strikes", "pitcherteam", "vertapprangle")]

unique(data2023_for_xba$playresult)

data2023xBABIP <- filter(data2023_for_xba, pitchcall == "InPlay")

data2023xBABIP$hit <- ifelse(data2023xBABIP$playresult == "HomeRun" | data2023xBABIP$playresult == "Triple" |
                               data2023xBABIP$playresult == "Double" | data2023xBABIP$playresult == "Single", 1, 0)

data2023xBABIP <- data2023xBABIP %>%
  filter(!is.na(hit),
         !is.na(exitspeed),
         !is.na(angle))

#------------Random Forest-----------
#Random Forest
RandomForestxBABIP <- randomForest(as.factor(hit) ~ exitspeed + angle,
                                   data = data2022xBABIP)

RandomForestxBABIP

predict <- predict(RandomForestxBABIP, data2023xBABIP, type = "prob")

xBABIP1 <- cbind(predict, data2023xBABIP)

#rename column and filter
colnames(xBABIP1)[colnames(xBABIP1) == "1"] <- "Prediction"

xBABIP1 <- xBABIP1[,-c(1)]

#add in non BIP 
Strikeouts <- filter(data2023_for_xba, strikes == 2)
Strikeouts <- filter(Strikeouts, pitchcall == "StrikeCalled" | pitchcall == "StrikeSwinging")

Strikeouts$Prediction <- ifelse(Strikeouts$pitchcall == "StrikeCalled" | 
                                Strikeouts$pitchcall == "StrikeSwinging" , 0, 1)

#Combine BIP and NonBIP datasets and get rid of hit column in BIP dataset
xBABIP1 <- xBABIP1[,-c(14)]

CombinedxAVG <- rbind(xBABIP1, Strikeouts)

#----------Change Pitch Type Names---------
CombinedxAVG$taggedpitchtype <- gsub("FourSeamFastBall", "Fastball", CombinedxAVG$taggedpitchtype)
CombinedxAVG$taggedpitchtype <- gsub("TwoSeamFastBall", "Sinker", CombinedxAVG$taggedpitchtype)
CombinedxAVG$taggedpitchtype <- gsub("OneSeamFastBall", "Sinker", CombinedxAVG$taggedpitchtype)
CombinedxAVG <- CombinedxAVG[CombinedxAVG$taggedpitchtype != "Undefined", ]
CombinedxAVG <- CombinedxAVG[CombinedxAVG$taggedpitchtype != ",", ]
CombinedxAVG <- CombinedxAVG[CombinedxAVG$taggedpitchtype != "Other", ]

#---------Group by Pitcher---------
PlayerxBA <- CombinedxAVG %>%
  group_by(pitcherid, pitcher, taggedpitchtype, pitcherteam) %>%
  summarise(xBA = mean(Prediction, na.rm = TRUE),
            Pitches_Considered_for_xBA = n(),
            VAA = mean(vertapprangle, na.rm = TRUE))
            
MiamixBA <- CombinedxAVG %>%
  group_by(pitcherid, pitcher, taggedpitchtype, pitcherteam) %>%
  filter(pitcherteam == "MIA_RED" | pitcher == "Olejnik, Peyton" |
           pitcher == "Harajli, Ahmad" | pitcher == "Galdoni, Lukas" | 
           pitcher == "Pauley, Cole" | pitcher == "Zimmer, Ryan") %>%
  summarise(xBA = mean(Prediction, na.rm = TRUE),
            Pitches_Considered_for_xBA = n(),
            VAA = mean(vertapprangle, na.rm = TRUE))

#--------Write PlayerxBA and MiamixBA to a CSV---------
write.csv(PlayerxBA, "xBA.csv")
write.csv(MiamixBA, "MiamixBA.csv")









