###############################################

## Creating Trackman Reports
## Will Paz
## 7.11.24

###############################################
# Load necessary packages
library(tidyverse)
library(patchwork)
library(gridExtra)
library(grid)
library(kableExtra)

###############################################
# Read in the data
df <- read_csv("20240317-McKieFieldStad-1_unverified.csv") # Change file as needed
glimpse(df)

###############################################

#-------- Find the pitchers -------------------

findPitchers <- function(df) {
  pitchers <- unique(df %>%
                       filter(PitcherTeam == "MIA_RED") %>% # Change if needed
                       pull(Pitcher)) 
  return(pitchers)
}

###############################################

#-------- Find the date -----------------------

findDate <- function(df) {
  date <- unique(df$Date)
  return(date)
}

###############################################

#----- Create pitcher specific dataframe ------

pitcherDF <- function(df, pitcher) {
  pitcherDF <- df %>% 
    filter(Pitcher == pitcher)
  return(pitcherDF)
}

###############################################

#------------------ NAME ----------------------

###############################################

#------------ Name reformatting ---------------

reformatName <- function(pitcher) {
  # Split the pitcher's name into Last and First
  names <- str_split(pitcher, ", ")[[1]]
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

###############################################

#--------- Create the table summary -----------

tableSummary <- function(df, pitcher, date) {
  
  # Reformat the name
  full_name <- reformatName(pitcher)
  
  # Reformat the date
  formatted_date <- adjustDate(date)
  
  # Aggregate Data
  FinalPitches <- df %>%
    group_by(TaggedPitchType) %>%
    rename("Pitch Type" = "TaggedPitchType") %>%
    summarise(
      Pitches = n(),
      Velocity = paste0(
        round(quantile(RelSpeed, 0.25, na.rm = TRUE), 1), "-", 
        round(quantile(RelSpeed, 0.75, na.rm = TRUE), 1), " T", 
        round(max(RelSpeed, na.rm = TRUE), 1)
      ),
      `Spin Rate` = mean(SpinRate, na.rm = TRUE),
      HB = mean(HorzBreak, na.rm = TRUE),
      IVB = mean(InducedVertBreak, na.rm = TRUE),
      Extension = mean(Extension, na.rm = TRUE),
      `Vertical Release` = mean(RelHeight, na.rm = TRUE),
      `Horizontal Release` = mean(RelSide, na.rm = TRUE),
      VAA = mean(VertApprAngle, na.rm = TRUE)
    ) %>%
    arrange(desc(Pitches)) %>%
    mutate_at(vars(-`Pitch Type`, -Velocity), list(~ round(., 1)))
  
  # Create the tableGrob object
  table <- tableGrob(FinalPitches, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, formatted_date, "Pitch Report")
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and table into a single grob
  table_with_title <- arrangeGrob(title, table, ncol = 1, heights = c(0.1, 1.0))
  
  return(table_with_title)
}

###############################################

#--------- Create the break chart -------------

breakChart <- function(df) {
  # Define a color palette with enough colors
  pitch_types <- unique(df$TaggedPitchType)
  colors <- c("Fastball" = "dodgerblue3"  , "Cutter" = "darkorange3", 
              "Curveball" = "darkred", "Sinker" = "goldenrod3", 
              "Slider" = "darkolivegreen3", "ChangeUp" = "darkorchid3",
              "Splitter" = "darkorchid3")
  
  breakChart <- ggplot(df, aes(x = HorzBreak,
                               y = InducedVertBreak,
                               color = TaggedPitchType)) +
    geom_point(size = 1.5, alpha = 0.7) +
    xlim(-22, 22) +
    ylim(-22, 22) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_minimal() +
    labs(title = "Break Chart",
         subtitle = "Pitcher's Perspective",
         x = "Horizontal Break",
         y = "Induced Vertical Break") +
    scale_color_manual(values = colors) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")
  return(breakChart)
}

###############################################

#--------- Create the location chart ----------

locationChart <- function(df) {
  # Define a color palette with enough colors
  pitch_types <- unique(df$TaggedPitchType)
  colors <- c("Fastball" = "dodgerblue3"  , "Cutter" = "darkorange3", 
              "Curveball" = "darkred", "Sinker" = "goldenrod3", 
              "Slider" = "darkolivegreen3", "ChangeUp" = "darkorchid3",
              "Splitter" = "darkorchid3")
  
  df$PlateLocSide <- df$PlateLocSide * -1 #reversed to get pitcher pov
  locationMap <- ggplot(data = df, mapping = aes(x = PlateLocSide,
                                                 y = PlateLocHeight,
                                                 color = TaggedPitchType))+
    xlim(c(2,-2)) +
    ylim(c(0,5)) +
    coord_fixed(0.8) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6,
             ymax = 3.5, color = "black", fill = "black", alpha = 0.2) +
    labs(title = "Location Map",
         subtitle = "Pitcher's Perspective",
         x = element_blank(),
         y = element_blank(),
         color = "Pitch Type") +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  locationMap <- locationMap + geom_point(alpha = 0.8, size = 2.0) 
  return(locationMap)
}

###############################################

#------------- Create the report --------------

createReports <- function(df) {
  pitchers <- findPitchers(df)
  date <- findDate(df)
  
  for (pitcher in pitchers) {
    # Creates a pitcher specific dataframe
    pitcher_df <- pitcherDF(df, pitcher)
    
    # Get the elements of the dashboard
    table <- tableSummary(pitcher_df, pitcher, date)
    breakChart <- breakChart(pitcher_df)
    locationChart <- locationChart(pitcher_df)
    
    # Sets the plot layout
    plot_layout <- "
    AA
    BC"
    
    # Wraps into the dashboard
    dashboard <- wrap_plots(table, locationChart, breakChart, design = plot_layout)
    
    # Save the dashboard as an image file in the current working directory
    full_name <- reformatName(pitcher)
    sanitized_name <- sanitizeName(full_name)
    sanitized_date <- sanitizeDate(date)
    file_name <- paste0(sanitized_name, "_Pitch_Profile_", sanitized_date, ".png")
    
    # Saves the image
    ggsave(filename = file_name[1], plot = dashboard, width = 12, height = 8)
  }
}

###############################################
# Create and save the reports in the current working directory
createReports(df)

