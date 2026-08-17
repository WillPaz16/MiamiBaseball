#########################################

## Fall Catch Chart
## Will Paz
## 11.6.24

#########################################

library(tidyverse)
library(sportyR)

#########################################

# Define key fence points with angles and distances
fence_data <- data.frame(
  angle = c(45, 0, -45),  # Angles (in degrees)
  distance = c(332, 400, 345)  # Distances (in feet)
) %>%
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

# Interpolate smooth points using spline with higher resolution
smooth_fence <- as.data.frame(spline(
  x = fence_data$x,
  y = fence_data$y,
  n = 1000  # Increase the number of points for a smoother curve
))



#########################################

fallDF <- read_csv("fallDF.csv")

catchChartDF <- fallDF %>% 
  filter(PlayResult != "Undefined",
         Distance > 155,
         !(TaggedHitType %in% c("GroundBall", "Popup"))) %>% 
  mutate(PlayResult = ifelse(PlayResult != "Out", "No Catch", "Catch"))

geom_baseball(league = "MLB") +
  geom_point(data = catchChartDF, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                                      y = round(Distance * cos(Bearing * pi / 180), 3),
                                      color = PlayResult,
                                      shape = TaggedHitType)) +
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  labs(title = "Catch Chart", x = "", y = "") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12))

catchChartDFLeft <- fallDF %>% 
  filter(PlayResult != "Undefined",
         Distance > 155,
         BatterSide == "Left",
         !(TaggedHitType %in% c("GroundBall", "Popup"))) %>% 
  mutate(PlayResult = ifelse(PlayResult != "Out", "No Catch", "Catch"))

geom_baseball(league = "MLB") +
  geom_point(data = catchChartDFLeft, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                                      y = round(Distance * cos(Bearing * pi / 180), 3),
                                      color = PlayResult,
                                      shape = TaggedHitType)) +
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  labs(title = "Catch Chart (Left-Handed Batter)", x = "", y = "") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12))

catchChartDFRight <- fallDF %>% 
  filter(PlayResult != "Undefined",
         Distance > 155,
         BatterSide == "Right",
         !(TaggedHitType %in% c("GroundBall", "Popup"))) %>% 
  mutate(PlayResult = ifelse(PlayResult != "Out", "No Catch", "Catch"))

geom_baseball(league = "MLB") +
  geom_point(data = catchChartDFRight, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                                          y = round(Distance * cos(Bearing * pi / 180), 3),
                                          color = PlayResult,
                                          shape = TaggedHitType)) +
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  labs(title = "Catch Chart (Right-Handed Batter)", x = "", y = "") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12))


