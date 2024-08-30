###############################################

## Creating Scully Reports
## Will Paz
## 7.16.24

###############################################
# Load necessary packages
library(tidyverse)
library(patchwork)
library(gridExtra)
library(grid)
library(kableExtra)
###############################################\
# Read in the data
df <- read_csv("20240317-McKieFieldStad-1_unverified.csv") # Change file as needed
glimpse(df)

###############################################

###############################################

#------------- Find the pitchers --------------

findPitchers <- function(df) {
  pitchers <- unique(df %>%
                       filter(PitcherTeam == "MIA_RED") %>% # Change if needed
                       pull(Pitcher)) 
  return(pitchers)
}

###############################################

#-------------- Find the date -----------------

findDate <- function(df) {
  date <- unique(df$Date)
  return(date)
}

###############################################

#----- Create pitcher specific dataframe ------

pitcherDF <- function(df, pitcher) {
  pitcherDF <- df %>% 
    filter(Pitcher == pitcher) %>% 
    mutate(paofinningupdate = paste0(Date, Inning, PAofInning))
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

#------ Sanitize the date for filename -------

sanitizeDate <- function(date) {
  # Reformat the date to mm_dd_yy format
  date <- as.Date(date)
  formatted_date <- format(date, "%m_%d_%y")
  
  return(formatted_date)
}

###############################################

#--------------WORKING AHEAD-------------------

###############################################

# -------- 0-1 counts that become 0-2 ---------

oneStrike <- function(pitcher_df) {
  pitcher_df <- pitcher_df %>%
    mutate(Count = ifelse(Balls == "0" & Strikes == "1", 1, 0),
           rowID = row_number())
  
  # Identify the rows where 0-1 becomes 0-2
  filtered_df <- pitcher_df %>%
    filter(Count == 1) %>%
    select(rowID) %>%
    mutate(nextRowID = rowID + 1) %>%
    pivot_longer(cols = c(rowID, nextRowID), names_to = "type", values_to = "rowID") %>%
    select(rowID) %>%
    distinct() %>%
    inner_join(pitcher_df, by = "rowID") %>%
    filter(!(Balls == 0 & Strikes == 0) & Count != 1) %>%
    mutate(nextCount = ifelse(Balls == "0" & Strikes == "2", 1, 0))
  
  if (nrow(filtered_df) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% 0-1 That Go 0-2` = "0%",
        pitchesConsidered1 = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- filtered_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% 0-1 That Go 0-2` = mean(nextCount, na.rm = TRUE) * 100,
        pitchesConsidered1 = n(),
        .groups = 'drop'
      ) %>%
      mutate(`% 0-1 That Go 0-2` = paste0(round(`% 0-1 That Go 0-2`, 2), "%"))
  }
  
  return(final_df)
}

###############################################

# -------- 1-1 counts that become 1-2 ---------

oneBallOneStrike <- function(pitcher_df) {
  pitcher_df <- pitcher_df %>%
    mutate(Count = ifelse(Balls == "1" & Strikes == "1", 1, 0),
           rowID = row_number())
  
  # Identify the rows where 1-2 becomes 1-2
  filtered_df <- pitcher_df %>%
    filter(Count == 1) %>%
    select(rowID) %>%
    mutate(nextRowID = rowID + 1) %>%
    pivot_longer(cols = c(rowID, nextRowID), names_to = "type", values_to = "rowID") %>%
    select(rowID) %>%
    distinct() %>%
    inner_join(pitcher_df, by = "rowID") %>%
    filter(!(Balls == 0 & Strikes == 0) & Count != 1) %>%
    mutate(nextCount = ifelse(Balls == "1" & Strikes == "2", 1, 0))
  
  if (nrow(filtered_df) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% 1-1 That Go 1-2` = "0%",
        pitchesConsidered2 = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- filtered_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% 1-1 That Go 1-2` = mean(nextCount, na.rm = TRUE) * 100,
        pitchesConsidered2 = n(),
        .groups = 'drop'
      ) %>%
      mutate(`% 1-1 That Go 1-2` = paste0(round(`% 1-1 That Go 1-2`, 2), "%"))
  }
  
  return(final_df)
}

###############################################

# ------------ First Pitch Strike -------------

firstPitchStrike <- function(pitcher_df) {
  filtered_df <- pitcher_df %>% 
    filter(PitchofPA == 1) %>%
    mutate(FirstPitchK = ifelse(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", 
                                                 "FoulBallNotFieldable", "InPlay"), 1, 0))
  
  if (nrow(filtered_df) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `1st Pitch Strike %` = "0%",
        pitchesConsidered3 = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- filtered_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `1st Pitch Strike %` = round(mean(FirstPitchK, na.rm = TRUE) * 100, 2),
        pitchesConsidered3 = n(),
        .groups = 'drop'
      ) %>%
      mutate(`1st Pitch Strike %` = paste(`1st Pitch Strike %`, "%", sep = ""))
  }
  
  return(final_df)
}

###############################################

# ------- First 2 of 3 Pitches Strikes --------

firstTwoOfThreeStrikes <- function(pitcher_df) {
  # Checks to see if the first two of three pitches in an AB are strikes
  final_df <- pitcher_df %>%
    group_by(paofinningupdate) %>%
    filter(n() >= 3) %>%
    slice_head(n = 3) %>%
    ungroup() %>% 
    group_by(paofinningupdate) %>%
    mutate(firstTwoOfThreeStrike = as.integer(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", 
                                                                   "FoulBall", "InPlay", 
                                                                   "FoulBallNotFieldable")) >= 2)) %>%
    group_by(PitcherId, Pitcher, paofinningupdate) %>%
    summarize(firstTwoOfThreeStrike = mean(firstTwoOfThreeStrike, na.rm = TRUE)) %>%
    group_by(PitcherId, Pitcher) %>%
    summarize(`First Two Of Three Strike %` = round(mean(firstTwoOfThreeStrike, na.rm = TRUE) * 100, 2),
              plateAppearances1 = n()) %>%
    mutate(`First Two Of Three Strike %` = paste(`First Two Of Three Strike %`, "%", sep = ""))
  return(final_df)
}

###############################################

#---------- Working ahead function ------------

workingAhead <- function(pitcher_df) {
  # Perform the joins
  new_df <- oneStrike(pitcher_df) %>%
    inner_join(oneBallOneStrike(pitcher_df)) %>%
    inner_join(firstPitchStrike(pitcher_df)) %>%
    inner_join(firstTwoOfThreeStrikes(pitcher_df)) %>% 
    select(-PitcherId)
  
  # Select percentage columns
  pct_df <- new_df %>% 
    select(Pitcher, `% 0-1 That Go 0-2`, `% 1-1 That Go 1-2`,
            `1st Pitch Strike %`, `First Two Of Three Strike %`)
  
  # Changes the pitcher's name to percentage for the aesthetic 
  pct_df$Pitcher[1] <- "Percentage"
  
  # New row for all of the pitches considered or plate appearances
  new_row <- c("Pitches or PA Considered", new_df$pitchesConsidered1[1], new_df$pitchesConsidered2[1],
               new_df$pitchesConsidered3[1], new_df$plateAppearances1[1])
  
  # Convert the new row to a data frame
  new_row_df <- as.data.frame(t(new_row), stringsAsFactors = FALSE)
  
  # Set column names for the new row data frame
  colnames(new_row_df) <- colnames(pct_df)
  
  # Combine the long data frames
  final_df <- rbind(pct_df, new_row_df) %>% 
    rename(" " = "Pitcher")
  
  return(final_df)
}

###############################################

#------------------ COMMAND -------------------

commandDF <- function(pitcher_df) {
  strike_vector <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay", "FoulBallNotFieldable")
  
  # Calculates strike percentage
  command_df <- pitcher_df %>%
    group_by(PitcherId, Pitcher, TaggedPitchType) %>%
    mutate(strike = if_else(PitchCall %in% strike_vector, 1, 0)) %>%
    summarize(`Strike %` = sum((strike)/n()),
              `Pitches Considered` = n()) %>% 
    ungroup() %>% 
    rename("Pitch Type" = "TaggedPitchType")
  
  command_df$`Strike %` <- round(command_df$`Strike %` * 100, 2)
  command_df$`Strike %` <- paste(command_df$`Strike %`, "%", sep = "")
  
  command_df <- command_df %>%
    arrange(desc(`Pitches Considered`)) %>% 
    mutate(PitcherId = NULL)
  return(command_df)
}

###############################################

#--------------- EFFICIENCY -------------------

###############################################

# ----------- Three Ball Counts ---------------

threeBallCounts <- function(pitcher_df) { 
  # Counts three ball counts
  ball3Counts <- pitcher_df %>%
    group_by(paofinningupdate) %>%
    mutate(
      `% of PA's that go to 3 Balls Count` = as.integer(any(Balls == 3))
    )
  
  if (nrow(ball3Counts) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(PitcherId, Pitcher) %>%
      summarize(
        `% of PA's that go to 3 Balls Count` = "0%",
        plateAppearances2 = 0,
        .groups = 'drop'
      )
  } else {
    ball3Counts <- ball3Counts %>%
      group_by(PitcherId, Pitcher, paofinningupdate) %>%
      summarize(`% of PA's that go to 3 Balls Count` = mean(`% of PA's that go to 3 Balls Count`, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(PitcherId, Pitcher) %>%
      summarize(`% of PA's that go to 3 Balls Count` = mean(`% of PA's that go to 3 Balls Count`, na.rm = TRUE),
                plateAppearances2 = n()) %>% 
      ungroup()
    
    ball3Counts$`% of PA's that go to 3 Balls Count` <- round(ball3Counts$`% of PA's that go to 3 Balls Count` * 100, 2)
    ball3Counts$`% of PA's that go to 3 Balls Count` <- paste(ball3Counts$`% of PA's that go to 3 Balls Count`, "%", sep = "")
    
    final_df <- ball3Counts
  }
  
  return(final_df)
}

###############################################

# ------- First Batter of Inning Out % --------

firstBatterOut <- function(pitcher_df) {
  # Checks if the first batter of an inning was called out
  firstBatterLastRow <- pitcher_df %>%
    filter(substr(paofinningupdate, nchar(paofinningupdate), nchar(paofinningupdate)) == "1") %>%
    group_by(paofinningupdate) %>%
    slice_tail(n = 1) %>%
    mutate(Out = as.integer(PlayResult == "Out" | PitchCall %in% c("StrikeSwinging", "StrikeCalled"))) %>%
    ungroup()
  
  if (nrow(firstBatterLastRow) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(PitcherId, Pitcher) %>%
      summarize(
        `1st Batter of Inning Out %` = "0%",
        plateAppearances3 = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- firstBatterLastRow %>%
      group_by(PitcherId, Pitcher) %>%
      summarize(
        `1st Batter of Inning Out %` = round(mean(Out, na.rm = TRUE) * 100, 2),
        plateAppearances3 = n(),
        .groups = 'drop'
      ) %>%
      mutate(`1st Batter of Inning Out %` = paste(`1st Batter of Inning Out %`, "%", sep = "")) %>% 
      ungroup()
  }
  
  return(final_df)
}

###############################################

# --------- 3 Pitches or less PA --------------

threeOrLess <- function(pitcher_df) { ## Consider if there's a result?
  result <- pitcher_df %>%
    group_by(paofinningupdate, Pitcher, PitcherId) %>%
    summarize(count = n(), .groups = 'drop') %>%
    mutate(threeOrLess = ifelse(count <= 3, 1, 0))
  
  threeOrLessPitchPA <- result %>%
    group_by(Pitcher, PitcherId) %>%
    summarize(`3 or Less Pitch PA %` = round(mean(threeOrLess, na.rm = TRUE) * 100, 2),
              plateAppearances4 = n(), .groups = 'drop') %>%
    mutate(`3 or Less Pitch PA %` = paste(`3 or Less Pitch PA %`, "%", sep = "")) %>% 
    ungroup()
  return(threeOrLessPitchPA)
}

###############################################

# --------- 4 Pitches or less PA --------------

fourOrLess <- function(pitcher_df) { ## Consider if there's a result?
  result <- pitcher_df %>%
    group_by(paofinningupdate, Pitcher, PitcherId) %>%
    summarize(count = n(), .groups = 'drop') %>%
    mutate(fourOrLess = ifelse(count <= 4, 1, 0))
  
  fourOrLessPitchPA <- result %>%
    group_by(Pitcher, PitcherId) %>%
    summarize(`4 or Less Pitch PA %` = round(mean(fourOrLess, na.rm = TRUE) * 100, 2),
              plateAppearances5 = n(), .groups = 'drop') %>%
    mutate(`4 or Less Pitch PA %` = paste(`4 or Less Pitch PA %`, "%", sep = "")) %>% 
    ungroup()
  return(fourOrLessPitchPA)
}

###############################################

#----------- Efficiency function --------------

effiencyDF <- function(pitcher_df) {
  # Join the effiency dataframes
  new_df <- inner_join(threeBallCounts(pitcher_df), firstBatterOut(pitcher_df)) %>% 
    inner_join(threeOrLess(pitcher_df)) %>% 
    inner_join(fourOrLess(pitcher_df)) %>% 
    mutate(PitcherId = NULL)
  
  # Select percentage columns
  pct_df <- new_df %>% 
    select(Pitcher, `% of PA's that go to 3 Balls Count`, `1st Batter of Inning Out %`,
           `3 or Less Pitch PA %`, `4 or Less Pitch PA %`)
  
  # Changes the pitcher's name to percentage for the aesthetic 
  pct_df$Pitcher[1] <- "Percentage"
  
  # New row for all of the pitches considered or plate appearances
  new_row <- c("PA Considered", new_df$plateAppearances2[1], new_df$plateAppearances3[1],
               new_df$plateAppearances4[1], new_df$plateAppearances5[1])
  
  # Convert the new row to a data frame
  new_row_df <- as.data.frame(t(new_row), stringsAsFactors = FALSE)
  
  # Set column names for the new row data frame
  colnames(new_row_df) <- colnames(pct_df)
  
  # Combine the long data frames
  final_df <- rbind(pct_df, new_row_df) %>% 
    rename(" " = "Pitcher")
  
  return(final_df)
}

###############################################

#----------------- MAKEUP ---------------------

###############################################

# ------ Percent of Runners that Score --------

scoringRunners <- function(pitcher_df) {
  scoring_df <- pitcher_df %>%
    mutate(Runners = if_else(PlayResult %in% c("Double", "Triple", "HomeRun", "Single", "Error") |
                               KorBB == "Walk" | PitchCall == "HitByPitch", 1, 0))
  
  if (sum(scoring_df$Runners, na.rm = TRUE) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(PitcherId, Pitcher) %>%
      summarize(
        `% of Runners That Score` = "0%",
        runnersConsidered = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- scoring_df %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% of Runners That Score` = round(sum(RunsScored, na.rm = TRUE) / sum(Runners, na.rm = TRUE) * 100, 2),
        runnersConsidered = sum(Runners == 1, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(`% of Runners That Score` = paste(`% of Runners That Score`, "%", sep = ""))
  }
  
  return(final_df)
}

###############################################

# Percent of 2-0, 2-1, 3 Ball Count ends w/ outs

badCountOuts <- function(pitcher_df) {
  badCounts <- pitcher_df %>%
    group_by(paofinningupdate) %>%
    filter(
      any((Balls == 2 & Strikes %in% c(0, 1)) | Balls == 3)
    ) %>%
    slice_tail(n = 1) %>%
    mutate(Out = if_else(PlayResult == "Out" | KorBB == "Strikeout", 1, 0)) %>%
    ungroup()
  
  if (nrow(badCounts) == 0) { # Check to make sure there are instances
    final_df <- pitcher_df %>%
      group_by(PitcherId, Pitcher) %>%
      summarize(
        `% of 2-0, 2-1, 3 Ball Counts Resulting In Outs` = "0%",
        plateAppearances6 = 0,
        .groups = 'drop'
      )
  } else {
    final_df <- badCounts %>%
      group_by(Pitcher, PitcherId) %>%
      summarize(
        `% of 2-0, 2-1, 3 Ball Counts Resulting In Outs` = round(mean(Out, na.rm = TRUE) * 100, 2),
        plateAppearances6 = n(),
        .groups = 'drop'
      ) %>%
      mutate(`% of 2-0, 2-1, 3 Ball Counts Resulting In Outs` = paste(`% of 2-0, 2-1, 3 Ball Counts Resulting In Outs`, "%", sep = ""))
  }
  
  return(final_df)
}

###############################################

#------------- Makeup function ----------------

makeupDF <- function(pitcher_df) {
  # Join the dataframes
  new_df <- inner_join(scoringRunners(pitcher_df), badCountOuts(pitcher_df)) %>%
    mutate(PitcherId = NULL)
  
  # Select percentage columns
  pct_df <- new_df %>% 
    select(Pitcher, `% of Runners That Score`, `% of 2-0, 2-1, 3 Ball Counts Resulting In Outs`)
  
  # Changes the pitcher's name to percentage for the aesthetic 
  pct_df$Pitcher[1] <- "Percentage"
  
  # New row for all of the pitches considered or plate appearances
  new_row <- c("Runners or PA Considered", new_df$runnersConsidered[1], new_df$plateAppearances6[1])
  
  # Convert the new row to a data frame
  new_row_df <- as.data.frame(t(new_row), stringsAsFactors = FALSE)
  
  # Set column names for the new row data frame
  colnames(new_row_df) <- colnames(pct_df)
  
  # Combine the long data frames
  final_df <- rbind(pct_df, new_row_df) %>% 
    rename(" " = "Pitcher")
  
  return(final_df)
}

###############################################

#----------------- TABLES ---------------------

###############################################

#----------- Working Ahead Table --------------

workingAheadTable <- function(working_ahead_df, pitcher) {
  # Reformat the name
  full_name <- reformatName(pitcher)
  
  # Create the tableGrob object
  table <- tableGrob(working_ahead_df, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, "Working Ahead")
  
  # Create a textGrob for the title
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and tableGrob using grid.arrange
  table <- grid.arrange(title, table, ncol = 1, heights = c(0.05, 0.95))
  return(table)
}

###############################################

#------------- Command Table ------------------

commandTable <- function(command_df, pitcher) {
  # Reformat the name
  full_name <- reformatName(pitcher)
  
  # Create the tableGrob object
  table <- tableGrob(command_df, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, "Command")
  
  # Create a textGrob for the title
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and tableGrob using grid.arrange
  table <- grid.arrange(title, table, ncol = 1, heights = c(0.05, 0.95))
  return(table)
}

###############################################

#----------- Efficiency Table -----------------

effiencyTable <- function(effiency_df, pitcher) {
  # Reformat the name
  full_name <- reformatName(pitcher)
  
  # Create the tableGrob object
  table <- tableGrob(effiency_df, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, "Efficiency")
  
  # Create a textGrob for the title
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and tableGrob using grid.arrange
  table <- grid.arrange(title, table, ncol = 1, heights = c(0.05, 0.95))
  return(table)
}

###############################################

#------------- Makeup Table -------------------

makeupTable <- function(makeup_df, pitcher) {
  # Reformat the name
  full_name <- reformatName(pitcher)
  
  # Create the tableGrob object
  table <- tableGrob(makeup_df, rows = NULL)
  
  # Add a title to the table
  title_text <- paste(full_name, "Makeup")
  
  # Create a textGrob for the title
  title <- textGrob(title_text, gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Combine the title and tableGrob using grid.arrange
  table <- grid.arrange(title, table, ncol = 1, heights = c(0.05, 0.95))
  return(table)
}

###############################################

# --------- PUTTING IT ALL TOGETHER -----------

###############################################

#------------- Create the report --------------

createReports <- function(df) {
  pitchers <- findPitchers(df)
  date <- findDate(df)
  
  for (pitcher in pitchers) {
    pitcher_df <- pitcherDF(df, pitcher)
    
    # Creates the various tables
    working_ahead_table <- workingAheadTable(workingAhead(pitcher_df), pitcher)
    command_table <- commandTable(commandDF(pitcher_df), pitcher)
    effiency_table <- effiencyTable(effiencyDF(pitcher_df), pitcher)
    makeup_table <- makeupTable(makeupDF(pitcher_df), pitcher)
    
    # Sets the plot layout
    plot_layout1 <- "
      A
      B
      C
      D"
    
    # Creates the Scully graphic
    scullyGraphic <- wrap_plots(working_ahead_table, command_table, effiency_table, makeup_table,
                                 design = plot_layout1)
    
    # Save the dashboard as an image file in the current working directory
    full_name <- reformatName(pitcher)
    sanitized_name <- sanitizeName(full_name)
    sanitized_date <- sanitizeDate(date)
    file_name <- paste0(sanitized_name, "_Scully_Report_", sanitized_date, ".png")
    
    # Saves the image
    ggsave(filename = file_name[1], plot = scullyGraphic, width = 12, height = 8)
  }
}

###############################################
# Create and save the reports in the current working directory
createReports(df)

