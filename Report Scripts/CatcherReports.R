###############################################

## Catching Reports
## Parker Kuchulan
## 2.20.2025

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
df <- read_csv("Miami/Miami Baseball/CatcherReportStuff/20250215-RileyPark-1.csv") # Change file as needed
glimpse(df)

###############################################

#-------------- Find the Catchers -------------

findCatchers <- function(df) {
  catchers <- unique(df %>%
                       filter(CatcherTeam == "MIA_RED") %>% # Change if needed
                       pull(Catcher))  
  return(catchers)
}

###############################################

#---------------- Find the date ---------------

findDate <- function(df) {
  date <- unique(df$Date)
  return(date)
}

###############################################

#----- Create catcher specific dataframe ------

catcherDF <- function(df, catcher) {
  catcherDF <- df %>% 
    filter(Catcher == catcher) %>% 
    subset(TaggedPitchType != "Other") %>% 
    subset(TaggedPitchType != "Knuckleball") %>% 
    mutate(
      PlayResult = case_when(
        KorBB == "Walk" & PitchCall == "BallIntentional" ~ "IntentionalWalk",
        KorBB == "Walk" ~ "Walk",
        KorBB == "Strikeout" ~ "Strikeout",
        PitchCall == "HitByPitch" ~ "HitByPitch",
        TRUE ~ PlayResult
      ),
      ExitSpeed = ifelse(PitchCall == "HitByPitch", NA, ExitSpeed),
      Angle = ifelse(PitchCall == "HitByPitch", NA, Angle)
    )
  
  return(catcherDF)
}

###############################################

#------------------ NAME ----------------------

###############################################

#------------ Name reformatting ---------------

reformatName <- function(catcher) {
  # Split the catcher's name into Last and First
  names <- str_split(catcher, ", ")[[1]]
  first <- names[2]
  last <- names[1]
  full_name <- paste(first, last)
  return(full_name)
}

#------------ Name sanitizing ---------------

sanitizeName <- function(name) {
  # Replace spaces and special characters with underscores
  sanitized_name <- gsub("[^A-Za-z0-9]", "_", name)
  return(sanitized_name)
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


#--------------- TABLE ------------------------

###############################################

#--------- Create the table summary -----------

tableSummary <- function(df, catcher, date) {
  
  # Reformat the name
  full_name <- reformatName(catcher)
  
  # Reformat the date
  formatted_date <- adjustDate(date)
  
  
  df <- df %>%
    mutate(InStrikeZone = ifelse(
      PlateLocSide >= -0.85 & PlateLocSide <= 0.85 &  # h strike zone
        PlateLocHeight >= 1.6 & PlateLocHeight <= 3.5,  # v strike zone
      1,  # In Zone
      0   # Out of Zone
    ))
    
  # Aggregate Data
  FinalPitches <- df %>%
    #group_by(TaggedPitchType) %>%
    #rename("Pitch Type" = "TaggedPitchType") %>%
    summarise(
      `Pitches Caught` = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "BallCalled"), na.rm = TRUE),
      `CS Caught` = sum(PitchCall %in% c("StrikeCalled"), na.rm = TRUE),
      `Balls Caught` = `Pitches Caught` - `CS Caught`,
      `CS in Zone` = sum(PitchCall %in% c("StrikeCalled") & InStrikeZone == 1, na.rm = TRUE),
      `CS Gained` = sum(PitchCall %in% c("StrikeCalled") & InStrikeZone == 0, na.rm = TRUE),
      `% of CS Gained` = paste0(round((`CS Gained` / sum(PitchCall %in% c("StrikeCalled")))*100, 2),"%"),
      `Balls Added` = sum(PitchCall %in% c("BallCalled") & InStrikeZone == 1, na.rm = TRUE),
      `% of Balls Added` = paste0(round((`Balls Added` / sum(PitchCall %in% c("BallCalled")))*100, 2), "%"),
      `Net Strikes Created` = `CS Gained` - `Balls Added`
    )
  

  #colors <- ifelse(FinalPitches$`Net Strikes Created` >= 0, "springgreen", "firebrick1")
  
#  FinalPitches$`Net Strikes Created` <- mapply(
 #   function(value, color) textGrob(as.character(value), gp = gpar(col = color, fontface = "bold")),
  #  FinalPitches$`Net Strikes Created`, colors
#  )

  
  
  # Create the tableGrob object
  table <- tableGrob(FinalPitches, rows = NULL) 
  
  # Add a title to the table
  title_text <- paste(full_name, formatted_date, "Catcher Report")
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and table into a single grob
  table_with_title <- arrangeGrob(title, table, ncol = 1, heights = c(0.1, 1.0))
  
  return(table_with_title)
}

###############################################


#--------------- CHARTS -----------------------

###############################################

#--------- Create the location chart ----------

locationChart <- function(df) {
  # Define a color palette with enough colors
  #pitch_types <- unique(df$TaggedPitchType)
  #colors <- c("Fastball" = "dodgerblue3"  , "Cutter" = "darkorange3", 
   #           "Curveball" = "darkred", "Sinker" = "goldenrod3", 
    #          "Slider" = "darkolivegreen3", "ChangeUp" = "darkorchid3",
     #         "Splitter" = "pink3")
  
  #df$PlateLocSide <- df$PlateLocSide * -1 #reversed to get pitcher pov
  
  # creating pitch categories var
  df <- df %>% filter(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "BallCalled")) %>% 
    mutate(InStrikeZone = ifelse(
      PlateLocSide >= -0.85 & PlateLocSide <= 0.85 &  # h strike zone
        PlateLocHeight >= 1.6 & PlateLocHeight <= 3.5,  # v strike zone
      1,  # In Zone
      0   # Out of Zone
    ),
    PitchCategory = case_when(
      PitchCall == "StrikeCalled" & InStrikeZone == 1 ~ "InZoneCalledStrike",
      PitchCall == "StrikeCalled" & InStrikeZone == 0 ~ "OutOfZoneCalledStrike",
      PitchCall == "BallCalled" & InStrikeZone == 1 ~ "InZoneBall",
      PitchCall == "BallCalled" & InStrikeZone == 0 ~ "OutOfZoneBall",
      PitchCall == "StrikeSwinging" ~ "StrikeSwinging"))
  
  df <- df %>%
    mutate(InStrikeZone = ifelse(
      PlateLocSide >= -0.85 & PlateLocSide <= 0.85 &  # h strike zone
        PlateLocHeight >= 1.6 & PlateLocHeight <= 3.5,  # v strike zone
      1,  # In Zone
      0   # Out of Zone
    ),
    PitchCategory = case_when(
      PitchCall == "StrikeCalled" & InStrikeZone == 1 ~ "True Called Strike",
      PitchCall == "StrikeCalled" & InStrikeZone == 0 ~ "Strike Gained",
      PitchCall == "BallCalled" & InStrikeZone == 1 ~ "Ball Added",
      PitchCall == "BallCalled" & InStrikeZone == 0 ~ "True Ball",
      PitchCall == "StrikeSwinging" ~ "Strike Swinging"
    ))
  
  colors <- c("Strike Gained" = "red3", "Ball Added" = "blue3", "True Called Strike" = "lavender",
              "True Ball" = "lavender", "Strike Swinging" = "lavender")

  locationMap <- ggplot(data = df, mapping = aes(x = PlateLocSide,
                                                 y = PlateLocHeight,
                                                 color = PitchCategory))+
    xlim(c(2,-2)) +
    ylim(c(0,5)) +
    coord_fixed(0.8) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6,
             ymax = 3.5, color = "black", fill = "black", alpha = 0.1) +
    labs(title = "Location Map",
         subtitle = "Catcher's Perspective",
         x = element_blank(),
         y = element_blank(),
         color = "Pitch Type") +
    scale_color_manual(
      breaks = c("Strike Gained", "Ball Added"),
      values = colors
    ) + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  locationMap <- locationMap + geom_point(alpha = 0.8, size = 2.0) 
  return(locationMap)
}

###############################################

#------------- Create the report --------------

createReports <- function(df) {
  catchers <- findCatchers(df)
  date <- findDate(df)
  
  for (catcher in catchers) {
    # Creates a catcher specific dataframe
    catcher_df <- catcherDF(df, catcher)
    
    # Get the elements of the dashboard
    table <- tableSummary(catcher_df, catcher, date)
    locationChart <- locationChart(catcher_df)
    
    # Sets the plot layout
    plot_layout <- "
    AAA
    BBB"
    
    # Wraps into the dashboard
    dashboard <- wrap_plots(table, locationChart, design = plot_layout)
    
    # Save the dashboard as an image file in the current working directory
    full_name <- reformatName(catcher)
    sanitized_name <- sanitizeName(full_name)
    sanitized_date <- sanitizeDate(date)
    file_name <- paste0(sanitized_name, "_Catcher_Report_", sanitized_date, ".png")
    
    # Saves the image
    ggsave(filename = file_name[1], plot = dashboard, width = 12, height = 8)
  }
}

###############################################
# Create and save the reports in the current working directory
createReports(df)

