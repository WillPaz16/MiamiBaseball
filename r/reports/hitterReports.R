###############################################

## Creating Hitter Reports
## Will Paz
## 7.28.24

###############################################
# Load necessary packages
library(tidyverse)
library(patchwork)
library(gridExtra)
library(grid)
library(kableExtra)
library(sportyR)

# AB 1 Table | Pitch Location 
# ...
# AB n Table | Pitch Location
# Spray Chart

###############################################

#------------- PREPROCESSING ------------------

###############################################
# Read in the data
df <- read_csv("20240317-McKieFieldStad-1_unverified.csv") # Change file as needed
glimpse(df)

###############################################

#------------- Find the batters ---------------

findBatters <- function(df) {
  batters <- unique(df %>%
                       filter(BatterTeam == "MIA_RED") %>% # Change if needed
                       pull(Batter)) 
  return(batters)
}

###############################################

#------------- Find the date ------------------

findDate <- function(df) {
  date <- unique(df$Date)
  return(date)
}

###############################################

#----- Create batter specific dataframe -------

batterDF <- function(df, batter) {
  df %>%
    filter(Batter == batter) %>%
    mutate(
      HardHit = ifelse(ExitSpeed >= 95, 1, 0),
      Count = str_c(Balls, "-", Strikes),
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
}

###############################################

#------------------ NAME ----------------------

###############################################

#------------ Name reformatting ---------------

reformatName <- function(batter) {
  # Split the batter's name into Last and First
  names <- str_split(batter, ", ")[[1]]
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

#------------------ TABLE ---------------------

###############################################

#------ Create the at bat table summary -------

tableSummary <- function(df, batter, date) {
  
  # Reformat the name
  full_name <- reformatName(batter)
  
  # Reformat the date
  formatted_date <- adjustDate(date)
  
  # Create a column to identify the end of an at-bat
  df <- df %>%
    mutate(AB_end = ifelse(PlayResult != "Undefined", 1, 0),
           AB = cumsum(AB_end)) 
  
  # Filter the dataframe to keep only the rows where AB ends
  AB <- df %>%
    filter(AB_end == 1) %>%
    summarize(AB = AB, 
              Pitcher = Pitcher, 
              Throws = PitcherThrows,
              `No.` = PitchofPA,
              Count = Count,
              Result = PlayResult,
              `Hit Type` = ifelse(!is.na(AutoHitType), AutoHitType, "--"),
              `Exit Velo` = ifelse(!is.na(ExitSpeed), round(ExitSpeed, 1), "--"),
              LA = ifelse(!is.na(Angle), round(Angle, 1), "--"),
              Distance = ifelse(!is.na(Distance), round(Distance, 1), "--"))
  
  # Create the tableGrob object
  table <- tableGrob(AB, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, formatted_date, "Batter Report")
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and table into a single grob
  table_with_title <- arrangeGrob(title, table, ncol = 1, heights = c(0.1, 1.0))
  
  return(table_with_title)
}

tableSummary(batterDF(df, "MacDonald, Zach"), "MacDonald, Zach", NA)

###############################################

#----------------- CHARTS ---------------------

###############################################

#--------- Create the location chart ----------

locationChart <- function(df) {
  pitch_types <- unique(df$TaggedPitchType)
  colors <- c("Fastball" = "dodgerblue3", "Cutter" = "darkorange3", 
              "Curveball" = "darkred", "Sinker" = "goldenrod3", 
              "Slider" = "darkolivegreen3", "ChangeUp" = "darkorchid3",
              "Splitter" = "darkorchid3")
  
  df$PlateLocSide <- df$PlateLocSide * -1 #reversed to get pitcher pov
  # Create the base plot
  locationMap <- ggplot(data = df, aes(x = PlateLocSide, y = PlateLocHeight, 
                                       color = TaggedPitchType, shape = PitchCall)) +
    xlim(c(2,-2)) +
    ylim(c(0,5)) +
    coord_fixed(0.8) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, color = "black", fill = "black", alpha = 0.2) +
    labs(title = "Location Map",
         subtitle = "Pitcher's Perspective",
         x = "Horizontal Location (ft)",
         y = "Vertical Location (ft)",
         color = "Pitch Type",
         shape = "Pitch Result") +  # Updated to include PitchCall in the legend
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  # Add points with varying shapes based on PitchCall
  locationMap <- locationMap + geom_point(alpha = 0.8, size = 3)
  
  return(locationMap)
}

locationChart(batterDF(df, "MacDonald, Zach"))

###############################################

#---------- Create the spray chart ------------

sprayChart <- function(df) {
  df <- df %>% 
    filter(PlayResult != "Undefined")
  
  spray <- geom_baseball(league = "MLB") +
    geom_point(data = df, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                              y = round(Distance * cos(Bearing * pi / 180), 3),
                              color = ExitSpeed)) +
    geom_text(data = df, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                             y = round(Distance * cos(Bearing * pi / 180), 3),
                             label = PlayResult), 
              vjust = -1, size = 4) +  # Adjust `vjust` and `size` as needed
    geom_text(data = df, aes(x = round(Distance * sin(Bearing * pi / 180), 3), 
                             y = round(Distance * cos(Bearing * pi / 180), 3),
                             label = paste0(round(Distance, 1), " ft")), 
              vjust = 2, size = 4) +  # Adjust `vjust` and `size` as needed
    scale_color_gradient(low = 'white', high = 'firebrick1') +
    labs(title = "Spray Chart", x = "", y = "") + 
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text = element_blank(),
          legend.position = "right",
          legend.text = element_text(size = 12))
  
  return(spray)
}

sprayChart(batterDF(df, "MacDonald, Zach"))

###############################################

#----------------- REPORT ---------------------

###############################################

createReports <- function(df) {
  batters <- findBatters(df)
  date <- findDate(df)
  
  for (batter in batters) {
    # Creates a batter-specific dataframe
    batter_df <- batterDF(df, batter)
    
    # Get the elements of the dashboard
    table <- tableSummary(batter_df, batter, date)
    location_plot <- locationChart(batter_df)
    spray_chart <- sprayChart(batter_df)
    
    # Sets the plot layout
    plot_layout <- "
    AA
    BC"
    
    # Set the plot layout using a grid
    dashboard <- wrap_plots(table, location_plot, spray_chart, design = plot_layout)
    
    # Save the dashboard as an image file
    full_name <- reformatName(batter)
    sanitized_name <- sanitizeName(full_name)
    sanitized_date <- sanitizeDate(date)
    file_name <- paste0(sanitized_name, "_Hitter_Report_", sanitized_date, ".png")
    
    # Save the image
    ggsave(filename = file_name, plot = dashboard, width = 12, height = 8)
  }
}


createReports(df)
