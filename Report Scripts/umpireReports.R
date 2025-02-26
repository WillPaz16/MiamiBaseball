###############################################

## Umpire Reports
## Will Paz
## 2.25.25

###############################################
# Load necessary packages
library(tidyverse)
library(patchwork)
library(gridExtra)
library(grid)
library(kableExtra)

###############################################

#------------- PREPROCESSING ------------------

###############################################
# Read in the data
df <- read_csv("~/Downloads/Miami University/Miami Baseball/Miami Shiny/2025 Season/20250225-McKieFieldStad-1_unverified.csv") # Change as needed

###############################################

#---------------- Find the date ---------------

findDate <- function(df) {
  date <- unique(df$Date)
  return(date)
}

###############################################

#------------------ DATE ----------------------

###############################################

#------------ Adjust the date -----------------

adjustDate <- function(date) {
  # Reformat the date to mm/dd/yy format
  date <- as.Date(date)
  formatted_date <- format(date, "%m/%d/%y")
  
  # Adjust formatted date to remove leading zero if present
  formatted_date <- gsub("0([1-9]/)", "\\1", formatted_date)
  
  return(formatted_date)
}

#------------ Sanitize the date for filename -----------------

sanitizeDate <- function(date) {
  # Reformat the date to mm_dd_yy format
  date <- as.Date(date)
  formatted_date <- format(date, "%m_%d_%y")
  
  return(formatted_date)
}

###############################################

#--------------- CHARTS -----------------------

###############################################

#--------- Create the location chart ----------

locationChart <- function(df) {
  df <- df %>% 
    filter(PitchCall %in% c("StrikeCalled", "BallCalled"))
  
  locationMap <- ggplot(data = df, mapping = aes(x = PlateLocSide,
                                                 y = PlateLocHeight,
                                                 color = PitchCall)) +
    xlim(c(2, -2)) +
    ylim(c(0, 5)) +
    coord_fixed(0.8) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6,
             ymax = 3.5, color = "black", fill = "black", alpha = 0.2) +
    labs(title = paste0("Location Map for ", format(findDate(df), "%m/%d/%y")),
         subtitle = "Umpire's Perspective",
         x = NULL,
         y = NULL,
         color = "Pitch Call") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    geom_point(alpha = 0.8, size = 4) +
    scale_color_manual(values = c("StrikeCalled" = "navy", "BallCalled" = "forestgreen"))
  
  return(locationMap)
}


###############################################

#------------- Create the report --------------

createReports <- function(df) {
  date <- findDate(df)
  
  # Get the elements of the dashboard
  locationChart <- locationChart(df)
  
  sanitized_date <- sanitizeDate(date)
  file_name <- paste0("Miami_Baseball_Umpire_Report_", sanitized_date, ".png")
  
  # Saves the image
  ggsave(filename = file_name[1], plot = locationChart, width = 12, height = 8)
}

###############################################
# Create and save the reports in the current working directory
createReports(df)

