#Load libraries
library(dplyr)
library(xgboost)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(gridExtra)
library(gridGraphics)
library(vip)
library(kableExtra)
library(knitr)
set.seed(1234)

#Load in the Data
data2023 <- read.csv("2023_College_Data.csv")
data2022 <- read.csv("data_2022.csv")
LinearWeights <- read.csv("MLB_LinearWeight_For_TM.csv")

#Remove game_year
data2022 <- data2022 %>% select(-game_year)

#player data
player_data <- filter(data2023, pitcher == "Marquez, Lazaro")

data20231 <- filter(data2023, pitcherteam == "MIA_RED")

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

#Add pitch_type to the final dataset
combined$taggedpitchtype <- testing$taggedpitchtype

#Clean up tagged names
combined$taggedpitchtype <- gsub("Four-Seam", "Fastball", combined$taggedpitchtype)
combined$taggedpitchtype <- gsub("FourSeamFastBall", "Fastball", combined$taggedpitchtype)
combined$taggedpitchtype <- gsub("TwoSeamFastBall", "Sinker", combined$taggedpitchtype)
combined$taggedpitchtype <- gsub("Changeup", "ChangeUp", combined$taggedpitchtype)
combined$taggedpitchtype <- gsub("OneSeamFastBall", "Sinker", combined$taggedpitchtype)
combined <- combined[combined$taggedpitchtype != "Undefined", ]
combined <- combined[combined$taggedpitchtype != ",", ]
combined <- combined[combined$taggedpitchtype != "Other", ]

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

PredictionFinal <- filter(PredictionFinal, Pitches >= 50 | pitcher == "Marquez, Lazaro")

unique(PredictionFinal$taggedpitchtype)

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

#Select Only the Player We Want
Pitches <- rbind(FBs4S, SLIndyScaled, CBIndyScaled, CHIndyScaled, SI, CT)
Pitches <- filter(Pitches, pitcher == "Marquez, Lazaro")

#-------------------Add in Whiffs, Strike %, In-Zone %------------------
unique(data2023_new$pitchcall)

#Alter pitchcall
player_data$pitchcall <- gsub("SwinginStrike", "StrikeSwinging", player_data$pitchcall)
player_data$pitchcall <- gsub("StriekSwinging", "StrikeSwinging", player_data$pitchcall)
player_data$pitchcall <- gsub("WildPitch", "BallCalled", player_data$pitchcall)
player_data$pitchcall <- gsub("BallinDirt", "BallCalled", player_data$pitchcall)

#Alter tagging
player_data$taggedpitchtype <- gsub("Four-Seam", "Fastball", player_data$taggedpitchtype)
player_data$taggedpitchtype <- gsub("FourSeamFastBall", "Fastball", player_data$taggedpitchtype)
player_data$taggedpitchtype <- gsub("TwoSeamFastBall", "Sinker", player_data$taggedpitchtype)
player_data$taggedpitchtype <- gsub("Changeup", "ChangeUp", player_data$taggedpitchtype)
player_data$taggedpitchtype <- gsub("OneSeamFastBall", "Sinker", player_data$taggedpitchtype)
player_data <- player_data[player_data$taggedpitchtype != "Undefined", ]
player_data <- player_data[player_data$taggedpitchtype != ",", ]
player_data <- player_data[player_data$taggedpitchtype != "Other", ]

# Assuming 'player' is your data frame
player_data <- player_data[player_data$taggedpitchtype != "Undefined", ]

csw_vector <- c("StrikeCalled", "StrikeSwinging", "FoulBall")
swings_vector <- c("InPlay", "StrikeSwinging", "FoulBall")
whiffs_vector <- c("StrikeSwinging")
strike_vector <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay")

Pitches1 <- player_data %>%
  group_by(taggedpitchtype, pitcher) %>%
  mutate(csw = if_else(pitchcall %in% csw_vector, 1, 0),
         swings = if_else(pitchcall %in% swings_vector, 1, 0),
         whiff = if_else(pitchcall %in% whiffs_vector, 1, 0),
         strike = if_else(pitchcall %in% strike_vector, 1, 0)) %>%
  summarise(Velocity = mean(relspeed, na.rm = TRUE),
            Spin_Rate = mean(spinrate, na.rm = TRUE),
            Horizontal_Break = mean(horzbreak, na.rm = TRUE),
            Vertical_Break = mean(inducedvertbreak, na.rm = TRUE),
            Extension = mean(extension, na.rm = TRUE),
            Vertical_Release = mean(relheight, na.rm = TRUE),
            Horizontal_Release = mean(relside, na.rm = TRUE),
            VAA = mean(vertapprangle, na.rm = TRUE),
            Strike = sum((strike)/n()) * 100,
            csw = sum(csw) / n() *100,
            swings = sum(swings),
            whiffs = sum((whiff) / swings) *100,
            Pitches = n())

FinalPitches <- merge(Pitches, Pitches1, by = "taggedpitchtype", all = TRUE)

FinalPitches <- subset(FinalPitches, select = c(pitcher.x, taggedpitchtype, 
                                                Pitches.x, Velocity.x, 
                                                Horizontal_Break.y, Vertical_Break.x, VAA,
                                                Spin_Rate, Strike, csw, whiffs, `Stuff+`)) %>%
  mutate_at(vars(-pitcher.x, -taggedpitchtype), list(~ round(., 1)))

# convert columns to numeric
FinalPitches$csw <- as.numeric(FinalPitches$csw)
FinalPitches$whiffs <- as.numeric(FinalPitches$whiffs)
FinalPitches$Strike <- as.numeric(FinalPitches$Strike)

# Add percentage sign to the numeric columns
FinalPitches$csw <- paste0(FinalPitches$csw, "%")
FinalPitches$whiffs <- paste0(FinalPitches$whiffs, "%")
FinalPitches$Strike <- paste0(FinalPitches$Strike, "%")


# Rename Columns
colnames(FinalPitches) <- c("Player Name", "Pitch Type", "Pitches", "Velocity", "HB",
                            "VB", "VAA", "Spin Rate", "Strike%", "CSW%", "Whiff%", "Stuff+")

FinalPitches <- FinalPitches %>% 
  arrange(desc(Pitches))

# Create the tableGrob object
table <- tableGrob(FinalPitches, rows = NULL)

# Create a textGrob for the title
title <- textGrob("Lazaro Marquez 2023 Report", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table <- grid.arrange(title, table, ncol = 1, heights = c(0.1, 1.0))

#------------------Pitch Break Chart-------------------
colors <- c("Fastball" = "dodgerblue3", "Cutter" = "darkorange3",
            "Curveball" = "darkred", "Sinker" = "goldenrod3",
            "Slider" = "darkolivegreen3", "Sweeper" = "mediumpurple3",
            "ChangeUp" = "darkorchid3", "Splitter" = "deeppink3")

BreakChart <- ggplot(player_data, aes(x = horzbreak, y = inducedvertbreak, color = taggedpitchtype))+
  geom_point(size = 0.5, alpha = 0.7)+
  xlim(-22, 22)+
  ylim(-22, 22)+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  theme_minimal()+
  xlab("Horizontal Break")+
  ylab("Vertical Break")+
  labs(title = "Break Chart",
       subtitle = "Pitcher's Perspective")+
  scale_color_manual(values = colors)

BreakChart


#-----------------Pie Charts----------------
Even1 <- filter(player_data, balls == 0 & strikes == 0 | balls == 1 & strikes == 1 | balls == 2 & strikes == 2)

Even1 <- Even1 %>%
  group_by(taggedpitchtype) %>%
  summarise(counts = n(),
            percentage = n()/nrow(Even1))


Behind1 <- filter(player_data, balls == 3 & strikes == 0 | balls == 2 & strikes == 0 | balls == 1 & strikes == 0 |
                    balls == 2 & strikes == 1 | balls == 3 & strikes == 1 | balls == 3 & strikes == 2)

Behind1 <- Behind1 %>%
  group_by(taggedpitchtype) %>%
  summarise(counts = n(),
            percentage = n()/nrow(Behind1))


Ahead1 <- filter(player_data, balls == 0 & strikes == 2 | balls == 1 & strikes == 2 | balls == 0 & strikes == 1)


Ahead1 <- Ahead1 %>%
  group_by(taggedpitchtype) %>%
  summarise(counts = n(),
            percentage = n()/nrow(Ahead1))


TwoStrikes1 <- filter(player_data, balls == 0 & strikes == 2 | balls == 1 & strikes == 2 |
                        balls == 2 & strikes == 2 | balls == 3 & strikes == 2)


TwoStrikes1 <- TwoStrikes1 %>%
  group_by(taggedpitchtype) %>%
  summarise(counts = n(),
            percentage = n()/nrow(TwoStrikes1))

#Even
Even1 <- ggplot(data = Even1, aes(x="", y = percentage, fill = taggedpitchtype)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(round(percentage*100), "%")), size = 3,
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none") +
  labs(title = "Even Count Chart") +
  scale_fill_manual(values = colors, guide = FALSE) +
  theme(plot.title = element_text(size = 10))

Even1


#Behind
Behind1 <- ggplot(data = Behind1, aes(x="", y = percentage, fill = taggedpitchtype)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(round(percentage*100), "%")), size = 3,
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  labs(title = "Behind Count Chart") +
  scale_fill_manual(values = colors, guide = FALSE) +
  theme(plot.title = element_text(size = 10))

Behind1

#Ahead
Ahead1 <- ggplot(data = Ahead1, aes(x="", y = percentage, fill = taggedpitchtype)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(round(percentage*100), "%")), size = 3,
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none") +
  labs(title = "Ahead Count Chart") +
  scale_fill_manual(values = colors, guide = FALSE) +
  theme(plot.title = element_text(size = 10))

Ahead1

#TwoStrike
TwoStrikes1 <- ggplot(data = TwoStrikes1, aes(x="", y = percentage, fill = taggedpitchtype)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(round(percentage*100), "%")), size = 3,
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none") +
  labs(title = "Two Strike Count Chart") +
  scale_fill_manual(values = colors, guide = FALSE) +
  theme(plot.title = element_text(size = 10))

TwoStrikes1

#-------------Point Maps---------------
player_data$platelocside <- player_data$platelocside * -1
LocationMap <- ggplot(data = player_data, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype))+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, color = "black", fill = "black", alpha = 0.3)+
  labs(title = "Location Map",
       subtitle = "Pitcher's Perspective",
       x = "",
       y = "")+
  scale_color_manual(values = colors)+
  theme_minimal()  

LocationMap <- LocationMap + geom_point(alpha = 0.5, size = 1.0) 


LocationMap

#----------------First Page Vizzy---------------
plot_layout <- "
  AAAA
  BBCC
  DEFG"

first_graphic <- wrap_plots(table, LocationMap, BreakChart, Ahead1, Behind1, Even1, TwoStrikes1,
           design = plot_layout)

ggsave("first_graphic.png", plot = first_graphic, width = 12, height = 8, units = "in")


#-----------------Heat Maps-------------------
#Fastball Heat Map
FF1 <- filter(player_data, taggedpitchtype == "Fastball")

FFMap <- ggplot(FF1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Fastball Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

FFMap

#Sinker Heat Map
SI1 <- filter(player_data, taggedpitchtype == "Sinker")

SIMap <- ggplot(SI1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Sinker Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

SIMap

#Cutter Heat Map
CT1 <- filter(player_data, taggedpitchtype == "Cutter")

CTMap <- ggplot(CT1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Cutter Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CTMap

#Changeup Map
CH1 <- filter(player_data, taggedpitchtype == "ChangeUp" | taggedpitchtype == "Splitter")

CHMap <- ggplot(CH1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Changeup Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CHMap

#Slider Map
SL1 <- filter(player_data, taggedpitchtype == "Slider")

SLMap <- ggplot(SL1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Slider Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

SLMap

#Curveball Map
CB1 <- filter(player_data, taggedpitchtype == "Curveball")

CBMap <- ggplot(CB1,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Curveball Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CBMap

#---------------Miss Point Plots----------------
datamiss <- filter(player_data, pitchcall == "StrikeSwinging")

#Fastball Miss Map
FF2 <- filter(datamiss, taggedpitchtype == "Fastball")

FF3 <- ggplot(FF2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Fastball Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

FF3

#Sinker Miss Map
SI2 <- filter(datamiss, taggedpitchtype == "Sinker")

SI3 <- ggplot(SI2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Sinker Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

SI3

#Changeup Miss Map
CH2 <- filter(datamiss, taggedpitchtype == "ChangeUp")

CH3 <- ggplot(CH2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Changeup Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CH3

#Cutter Heat Map
CT2 <- filter(datamiss, taggedpitchtype == "Cutter")

CT3 <- ggplot(CT2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Cutter Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CT3

#Slider
SL2 <- filter(datamiss, taggedpitchtype == "Slider")

SL3 <- ggplot(SL2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Slider Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

SL3

#Curveball
CB2 <- filter(datamiss, taggedpitchtype == "Curveball")

CB3 <- ggplot(CB2,aes(x = platelocside, y = platelocheight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  scale_fill_gradient(low = "blue", high = "red")+
  xlim(c(2,-2)) +
  ylim(c(0,5)) +
  coord_fixed(0.8)+
  labs(title = "Curveball Whiff Heat Map",
       subtitle = "Pitcher's Perspective",
       x = "", 
       y = "")+
  theme_minimal()+ 
  annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+ theme(legend.position = "none")

CB3

#-------------------Second Page Vizzy----------------
plot_layout1 <- "
  A
  B"

second_graphic <- wrap_plots(FFMap, FF3,
           design = plot_layout1)

ggsave("second_graphic.png", plot = second_graphic, width = 12, height = 6, units = "in")

