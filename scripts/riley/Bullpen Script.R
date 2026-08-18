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

#Load in Dataset
tm_df <- read.csv("20240317-McKieFieldStad-1_unverified.csv")

#---------Change Column Names and Clean----------

change_tm_columns <- function(tm_df) {
  
  char_cols <- c("Date","Time","Pitcher","PitcherId","PitcherThrows","PitcherTeam","Batter",
                 "BatterId","BatterSide","BatterTeam","PitcherSet","Top/Bottom",
                 "TaggedPitchType","AutoPitchType","PitchCall","KorBB","TaggedHitType",
                 "PlayResult","Notes","Tilt","HomeTeam","AwayTeam","Stadium","Level",
                 "League","GameID","PitchUID","GameUID","UTCDate","UTCTime","LocalDateTime",
                 "UTCDateTime","AutoHitType","System","HomeTeamForeignID","AwayTeamForeignID",
                 "Catcher","CatcherId","CatcherThrows","CatcherTeam","PlayID",
                 "PitchReleaseConfidence","PitchLocationConfidence","PitchMovementConfidence",
                 "HitLaunchConfidence","HitLandingConfidence","CatcherThrowCatchConfidence",
                 "CatcherThrowReleaseConfidence","CatcherThrowLocationConfidence")
  
  num_cols <- c("PitchNo","PAofInning","PitchofPA","Inning","Outs","Balls","Strikes",
                "OutsOnPlay","RunsScored","RelSpeed","VertRelAngle","HorzRelAngle","SpinRate",
                "SpinAxis","RelHeight","RelSide","Extension","VertBreak","InducedVertBreak",
                "HorzBreak","PlateLocHeight","PlateLocSide","ZoneSpeed","VertApprAngle",
                "HorzApprAngle","ZoneTime","ExitSpeed","Angle","Direction","HitSpinRate",
                "PositionAt110X","PositionAt110Y","PositionAt110Z","Distance","LastTrackedDistance",
                "Bearing","HangTime","pfxx","pfxz","x0","y0","z0","vx0","vy0","vz0",
                "ax0","ay0","az0","EffectiveVelo","MaxHeight","MeasuredDuration",
                "SpeedDrop","PitchLastMeasuredX","PitchLastMeasuredY","PitchLastMeasuredZ",
                "ContactPositionX","ContactPositionY","ContactPositionZ","PitchTrajectoryXc0",
                "PitchTrajectoryXc1","PitchTrajectoryXc2","PitchTrajectoryYc0","PitchTrajectoryYc1",
                "PitchTrajectoryYc2","PitchTrajectoryZc0","PitchTrajectoryZc1","PitchTrajectoryZc2",
                "HitSpinAxis","HitTrajectoryXc0","HitTrajectoryXc1","HitTrajectoryXc2","HitTrajectoryXc3",
                "HitTrajectoryXc4","HitTrajectoryXc5","HitTrajectoryXc6","HitTrajectoryXc7","HitTrajectoryXc8",
                "HitTrajectoryYc0","HitTrajectoryYc1","HitTrajectoryYc2","HitTrajectoryYc3","HitTrajectoryYc4",
                "HitTrajectoryYc5","HitTrajectoryYc6","HitTrajectoryYc7","HitTrajectoryYc8","HitTrajectoryZc0",
                "HitTrajectoryZc1","HitTrajectoryZc2","HitTrajectoryZc3","HitTrajectoryZc4","HitTrajectoryZc5",
                "HitTrajectoryZc6","HitTrajectoryZc7","HitTrajectoryZc8","ThrowSpeed","PopTime",
                "ExchangeTime","TimeToBase","CatchPositionX","CatchPositionY","CatchPositionZ",
                "ThrowPositionX","ThrowPositionY","ThrowPositionZ","BasePositionX","BasePositionY","BasePositionZ",
                "ThrowTrajectoryXc0","ThrowTrajectoryYc0","ThrowTrajectoryZc0","ThrowTrajectoryXc1",
                "ThrowTrajectoryYc1","ThrowTrajectoryYc2","ThrowTrajectoryXc2","ThrowTrajectoryZc1",
                "ThrowTrajectoryZc2")
  
  df <- tm_df %>%
    dplyr::mutate_at(vars(char_cols), ~as.character(.x))
  
  df <- df %>% 
    dplyr::mutate_at(vars(num_cols), ~as.numeric(as.character(.x)))
  
  
  return(df)
  
}


player_data <- filter(tm_df, Pitcher == "Harajili, Ahmad")

#Alter tagging
player_data$TaggedPitchType <- gsub("FourSeamFastBall", "Fastball", player_data$TaggedPitchType)
player_data$TaggedPitchType <- gsub("TwoSeamFastBall", "Sinker", player_data$TaggedPitchType)
player_data$TaggedPitchType <- gsub("Changeup", "ChangeUp", player_data$TaggedPitchType)
player_data$TaggedPitchType <- gsub("OneSeamFastBall", "Sinker", player_data$TaggedPitchType)
player_data <- player_data[player_data$TaggedPitchType != "Undefined", ]
player_data <- player_data[player_data$TaggedPitchType != ",", ]
player_data <- player_data[player_data$TaggedPitchType != "Other", ]
player_data <- player_data[player_data$TaggedPitchType != "Undefined", ]


#Aggregate Data
FinalPitches <- player_data %>%
  group_by(Pitcher, TaggedPitchType) %>%
  summarise(Pitches = n(),
            Velocity = mean(RelSpeed, na.rm = TRUE),
            Spin_Rate = mean(SpinRate, na.rm = TRUE),
            Horizontal_Break = mean(HorzBreak, na.rm = TRUE),
            Vertical_Break = mean(InducedVertBreak, na.rm = TRUE),
            Extension = mean(Extension, na.rm = TRUE),
            Vertical_Release = mean(RelHeight, na.rm = TRUE),
            Horizontal_Release = mean(RelSide, na.rm = TRUE),
            VAA = mean(VertApprAngle, na.rm = TRUE))

colnames(FinalPitches) <- c("Player Name", "Pitch Type", "Pitches", "Velocity", "Spin_Rate",
                            "HB", "VB", "Extension", "Vert_Rel", "Horz_Rel", "VAA")

FinalPitches <- FinalPitches %>% 
  arrange(desc(Pitches)) %>%
  mutate_at(vars(-"Player Name", -"Pitch Type"), list(~ round(., 1)))

# Create the tableGrob object
table <- tableGrob(FinalPitches, rows = NULL)

# Create a textGrob for the title
title <- textGrob("Ahmad Harajili 8/31/23 Bullpen Report", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table <- grid.arrange(title, table, ncol = 1, heights = c(0.1, 1.0))


#--------Pitch Break Chart-----------
colors <- c("Fastball" = "dodgerblue3", "Cutter" = "darkorange3",
            "Curveball" = "darkred", "Sinker" = "goldenrod3",
            "Slider" = "darkolivegreen3", "Sweeper" = "mediumpurple3",
            "ChangeUp" = "darkorchid3", "Splitter" = "deeppink3")

BreakChart <- ggplot(player_data, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType))+
  geom_point(size = 1.5, alpha = 0.7)+
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


#-------------Point Maps---------------
player_data$PlateLocSide <- player_data$PlateLocSide * -1 #reversed to get pitcher pov
LocationMap <- ggplot(data = player_data, mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType))+
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

LocationMap <- LocationMap + geom_point(alpha = 0.8, size = 2.0) 


LocationMap

#----------------First Page Vizzy---------------
plot_layout <- "
  AA
  BC"

first_graphic <- wrap_plots(table, LocationMap, BreakChart,
                            design = plot_layout)

ggsave("first_graphic.png", plot = first_graphic, width = 12, height = 8, units = "in")




