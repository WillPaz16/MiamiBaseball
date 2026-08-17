# Since sportyR package is not on CRAN, it needs to be explicitly installed for shiny to work (i think)

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("sportyR", quietly = TRUE)) {
  remotes::install_github("sportsdataverse/sportyR")
}

library(shiny)
library(readr)
library(lubridate)
library(tidyverse)
library(DT)
library(sportyR) 
library(rsconnect)
 
######### TO DO ###########

#* Filters
#*     Filter by AB
#*     Filter by opposing pitcher
#*     Issue: First table doesn't filter properly (works for batter and team but not per pitch, pitch type, side, etc..)
#*     
#* Add basic stat table (avg, slg, ops, etc.)
#* 
#* THE BIG ONE 
#*    Need swing decision/tendency section (discussion) 

############################

# csv read
game <- read.csv("~/Miami/Miami Baseball/ShinyApps/cleanedBatterGames.csv")
game$Date <- as.Date(game$Date)
game$HardHit <- ifelse(game$ExitSpeed >= 95, TRUE, FALSE)
game$TaggedPitchType <- factor(game$TaggedPitchType, levels = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter"))
pitch_colors <- c('Fastball' = '#d22d49', 'Sinker' = '#fe9d00', 'Cutter' = '#933f2c', 'Curveball' = '#00d1ed',
                  'Slider' = '#c3bd0d', 'Sweeper' = '#CB9AF0', 'ChangeUp' = '#23be41', 'Splitter' = '#3bacac', 'Other' = '#Acafaf')

# some useful functions
strike_zone <- c(-.9, .9, 1.55, 3.35)
is_in_zone <- function(height, side, zone = strike_zone) {
  zone <- 0
  for (i in seq_along(height)) {
    if (is.na(height[i]) | is.na(side[i])) {
      zone <- zone
    } else if (between(height[i], strike_zone[3], strike_zone[4]) &
               between(side[i], strike_zone[1], strike_zone[2])) {
      zone <- zone + 1
    }
  }
  return(zone)
}
is_swing <- function(pitch_call) {
  swings <- 0
  for (i in seq_along(pitch_call)) {
    if (pitch_call[i] %in% c('StrikeSwinging', 'FoulBall', 'InPlay')) {
      swings <- swings + 1
    }
  }
  return(swings)
}
is_o_swing <- function(height, side, pitch_call, zone = strike_zone) {
  o_swings <- 0
  for (i in seq_along(height)) {
    if ((!is_in_zone(height[i], side[i])) & is_swing(pitch_call[i])) {
      o_swings <- o_swings + 1
    }
  }
  return(o_swings)
}

convert_to_seconds <- function(hand, time) {
  time_parts <- strsplit(time, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  seconds <- as.numeric(time_parts[3])
  # If the hours equals 12 and is a righty, assume it is on the next day
  # without this, average of 12:30 and 1:00 spits junk value around 6:00,
  # instead of 12:45
  hours <- ifelse(hand == "Right" & hours == 12, 0, hours)
  hours <- ifelse(hand == "Right" & hours == 11, -1, hours)
  hours <- ifelse(hand == "Left" & hours == 1, 13, hours)
  return(hours * 3600 + minutes * 60 + seconds)
}

# Function to convert a numeric value representing the number of seconds to a time in HH:MM:SS format
convert_to_time <- function(seconds) {
  hours <- as.integer(floor(seconds / 3600))
  minutes <- as.integer(floor(seconds %% 3600 / 60))
  seconds <- as.integer(seconds %% 60)
  # If the time is 12:00:00 or later, assume it is on the next day
  hours <- ifelse(hours == 0, 12, hours)
  
  return(sprintf("%d:%02d", hours, minutes))
}

ui <- fluidPage(
  
  titlePanel("NCAA Batting Dashboard"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "PitcherTeamInput", label = "Select Pitcher Team", 
                  choices = c(All = "All", sort(unique(game$PitcherTeam)))),
      selectInput(inputId = "BatterTeamInput", label = "Select Batter Team", 
                  choices = c(All = "All", sort(unique(game$BatterTeam)))),
      selectInput(inputId = "BatterInput", label = "Select Batter", 
                  choices = c(All = "All", sort(unique(game$Batter)))),
      dateRangeInput(inputId = "DateRangeInput", 
                     label = "Select Date Range", 
                     start = as.Date(min(game$Date, na.rm = TRUE)), 
                     end = as.Date(max(game$Date, na.rm = TRUE)), 
                     min = min(game$Date, na.rm = TRUE), 
                     max = max(game$Date, na.rm = TRUE)),
      selectInput(inputId = "SplitInput", label = "Select Pitcher Hand", 
                  choices = c("Both", sort(unique(game$PitcherThrows)))),
      selectInput(inputId = "PitchInput", label = "Select Pitch", 
                  choices = c("All", "Primaries", "Breaking Balls", "OffSpeed", 
                              sort(levels(unique(game$TaggedPitchType)))), multiple = TRUE, selected = "All"),
      selectInput(inputId = "CountInput", label = "Select Count", 
                  choices = c("All", "Ahead", "Behind", "Even", 
                              sort(unique(game$Counts))), multiple = TRUE, selected = "All"),
      
      img(src = "NCAA_logo.png", 
          style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 200)),
    mainPanel(
      tabsetPanel(
        tabPanel("Sabermetric Batting", br(), dataTableOutput("summary_table"),
                 dataTableOutput("advanced_batter_table")),
        tabPanel("Batting Ball Results", br(), dataTableOutput("batted_ball_table"),
                 fluidRow(
                   column(4, plotOutput("hit_location_plot"), align = "center"),
                   column(4, plotOutput("spray_chart"), align = "center")
                 ))
        #        plotOutput("hit_location_plot"), plotOutput("spray_chart")),
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$BatterInput,# input$SplitInput, input$PitchInput, input$CountInput,
               updateSelectInput(session, inputId = "DateRangeInput", label = "Select Date Range", 
                                 choices = sort(unique(game$Date[game$Batter == input$BatterInput]))))
  
  output$selected_pitcher_team <- renderText({paste(input$PitcherTeamInput)})
  
  output$selected_hitting_team <- renderText({paste(input$BatterTeamInput)})
  
  output$selected_batter <- renderText({paste(input$BatterInput)})
  
  output$selected_game <- renderText({paste(input$DateRangeInput)})
  
  output$selected_hand <- renderText({paste(input$SplitInput)})
  
  output$selected_pitch <- renderText({paste(input$PitchInput)})
  
  output$selected_count <- renderText({paste(input$CountInput)})
  
  output$summary_table <- renderDataTable({
    table <- game
    
    if(input$PitcherTeamInput != "All") {
      table <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
    }
    
    if(input$BatterTeamInput != "All") {
      table <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
    }
    
    if(input$BatterInput != "All") {
      table <- table %>% filter(Batter %in% input$BatterInput)
    }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined") %>%
      dplyr::summarize('PA' = n(), 
                       BBE = sum(PitchCall == "InPlay", na.rm = TRUE),
                       H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE), 
                       `1B` = sum(PlayResult == "Single", na.rm = TRUE), 
                       `2B` = sum(PlayResult == "Double", na.rm = TRUE), 
                       `3B` = sum(PlayResult == "Triple", na.rm = TRUE), 
                       HR = sum(PlayResult == "HomeRun", na.rm = TRUE), 
                       SO = sum(PlayResult == "Strikeout", na.rm = TRUE), 
                       BB = sum(PlayResult == "Walk", na.rm = TRUE), 
                       HBP = sum(PlayResult == "HitByPitch", na.rm = TRUE))
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  ###############################################################################
  
  output$advanced_batter_table <- renderDataTable({
    table <- game
    table2 <- game
    
    if(input$PitcherTeamInput != "All") {
      table <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
      table2 <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
    }
    
    if(input$BatterTeamInput != "All") {
      table <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
      table2 <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
    }
    
    if(any(input$PitchInput == "All")){
      pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
    }
    else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
      input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")]
      input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
      input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
      if(length(input1) == 1){
        input1 <- c("Fastball", "Sinker")
      }
      if(length(input2) == 1){
        input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
      }
      if(length(input3) == 1){
        input3 <- c("ChangeUp", "Splitter")
      }
      pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
      pitchinput <- c(pitchinput, input1, input2, input3)
    }
    else{
      pitchinput = input$PitchInput
    }
    
    if(any(input$CountInput == "All")){
      countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
    }
    else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
      input4 <- input$CountInput[input$CountInput %in% c("Even")] 
      input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
      input6 <- input$CountInput[input$CountInput %in% c("Behind")]
      if(length(input4) == 1){
        input4 <- c("0-0", "1-1", "2-2")
      }
      if(length(input5) == 1){
        input5 <- c("0-1", "0-2", "1-2")
      }
      if(length(input6) == 1){
        input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
      }
      countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
      countinput <- c(countinput, input4, input5, input6)
    }
    else{
      countinput = input$CountInput
    }
    if(input$SplitInput == "Both"){
      splitinput = c("Right", "Left")
    }
    else{
      splitinput = input$SplitInput
    }
    if(input$BatterInput != "All") {
      table <- table %>% filter(Batter %in% input$BatterInput)
      table2 <- table2 %>% filter(Batter %in% input$BatterInput)
    }
    
    # ---------------------------------------------------------------------------
    
    # Pitch | PA | BBE | Usage % | K % | BB % | Chase % | Whiff % | wOBA | ISO
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitcherThrows %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput) %>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                       'PA' = sum(PlayResult != "Undefined"),
                       'BBE' = sum(PitchCall == "InPlay"),
                       in_zones = sum(is_in_zone(PlateLocHeight, PlateLocSide)),
                       out_zones = n() - in_zones,
                       chases = sum(is_o_swing(PlateLocHeight, PlateLocSide, PitchCall)),
                       "Chase %" = round(chases/out_zones*100, 1),
                       'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                           sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))*100,1),
                       'K %' = round(sum(PlayResult == "Strikeout")/PA*100, 1),
                       'BB %' = round(sum(PlayResult == "Walk")/PA*100, 1),
                       'wOBA' = round(((.693*sum(PlayResult == "Walk") + .693*sum(PlayResult == "HitByPitch") + .884*sum(PlayResult == "Single") + 1.261*sum(PlayResult == "Double") + 1.601*sum(PlayResult == "Triple") + 2.072*sum(PlayResult == "HomeRun"))/(PA-sum(PlayResult == "IntentionalWalk"))),3),
                       'ISO' = round((sum(PlayResult == "Double", na.rm = TRUE) + (2*sum(PlayResult == "Triple", na.rm = TRUE)) + (3*sum(PlayResult == "HomeRun", na.rm = TRUE))) / (sum(PlayResult != "Undefined") - sum(PlayResult %in% c("Walk", "HitByPitch", "IntentionalWalk", "Sacrifice"))),3)
      ) %>% 
      ungroup() %>%
      select(-c('No.', in_zones, out_zones, chases)) %>%
      select(Pitch, 'PA', 'BBE', 'K %', 'BB %', 'wOBA', 'ISO', 'Chase %', 'Whiff %')
    
    table2 <- table2 %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% pitchinput, Counts %in% countinput, PitcherThrows %in% splitinput) %>%
      dplyr::summarize('Pitch' = "Total",
                       #       'No.' = n(),
                       'PA' = sum(PlayResult != "Undefined"),
                       'BBE' = sum(PitchCall == "InPlay"),
                       in_zones = sum(is_in_zone(PlateLocHeight, PlateLocSide)),
                       out_zones = n() - in_zones,
                       chases = sum(is_o_swing(PlateLocHeight, PlateLocSide, PitchCall)),
                       "Chase %" = round(chases/out_zones*100, 1),
                       'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                           sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))*100,1),
                       'K %' = round(sum(PlayResult == "Strikeout")/PA*100, 1),
                       'BB %' = round(sum(PlayResult == "Walk")/PA*100, 1),
                       'wOBA' = round(((.693*sum(PlayResult == "Walk") + .693*sum(PlayResult == "HitByPitch") + .884*sum(PlayResult == "Single") + 1.261*sum(PlayResult == "Double") + 1.601*sum(PlayResult == "Triple") + 2.072*sum(PlayResult == "HomeRun"))/(PA-sum(PlayResult == "IntentionalWalk"))),3),
                       'ISO' = round((sum(PlayResult == "Double", na.rm = TRUE) + (2*sum(PlayResult == "Triple", na.rm = TRUE)) + (3*sum(PlayResult == "HomeRun", na.rm = TRUE))) / (sum(PlayResult != "Undefined") - sum(PlayResult %in% c("Walk", "HitByPitch", "IntentionalWalk", "Sacrifice"))),3)
      ) %>% 
      select(-c(in_zones, out_zones, chases)) %>%
      select(Pitch, 'PA', 'BBE', 'K %', 'BB %', 'wOBA', 'ISO', 'Chase %', 'Whiff %')
    
    table <- bind_rows(table, table2)
    
    aux <- nrow(table) - 1
    table$hiddenColumn <- 0
    table$hiddenColumn[aux] <- 1
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = c(0,ncol(table))))))  %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(3,5,7), `border-right` = "solid 1px") %>% 
      formatStyle(1:ncol(table), valueColumns = "hiddenColumn", `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatStyle('wOBA',
                  backgroundColor = styleInterval(c(.300, .340), c('lightcoral', 'white', 'lightgreen'))) %>%
      formatStyle('ISO',
                  backgroundColor = styleInterval(c(.120, .170), c('lightcoral', 'white', 'lightgreen'))) %>%
      formatStyle('Whiff %',
                  backgroundColor = styleInterval(c(20, 30), c('lightgreen', 'white', 'lightcoral'))) %>%
      formatStyle('Chase %',
                  backgroundColor = styleInterval(c(26, 30), c('lightgreen', 'white', 'lightcoral')))
    
  })
  
  ###############################################################################
  
  output$batted_ball_table <- renderDataTable({
    table <- game
    table2 <- game
    
    if(input$PitcherTeamInput != "All") {
      table <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
      table2 <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
    }
    
    if(input$BatterTeamInput != "All") {
      table <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
      table2 <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
    }
    
    if(any(input$PitchInput == "All")){
      pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
    }
    else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
      input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
      input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
      input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
      if(length(input1) == 1){
        input1 <- c("Fastball", "Sinker")
      }
      if(length(input2) == 1){
        input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
      }
      if(length(input3) == 1){
        input3 <- c("ChangeUp", "Splitter")
      }
      pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
      pitchinput <- c(pitchinput, input1, input2, input3)
    }
    else{
      pitchinput = input$PitchInput
    }
    
    if(any(input$CountInput == "All")){
      countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
    }
    else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
      input4 <- input$CountInput[input$CountInput %in% c("Even")] 
      input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
      input6 <- input$CountInput[input$CountInput %in% c("Behind")]
      if(length(input4) == 1){
        input4 <- c("0-0", "1-1", "2-2")
      }
      if(length(input5) == 1){
        input5 <- c("0-1", "0-2", "1-2")
      }
      if(length(input6) == 1){
        input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
      }
      countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
      countinput <- c(countinput, input4, input5, input6)
    }
    else{
      countinput = input$CountInput
    }
    if(input$SplitInput == "Both"){
      splitinput = c("Right", "Left")
    }
    else{
      splitinput = input$SplitInput
    }
    if(input$BatterInput != "All") {
      table <- table %>% filter(Batter %in% input$BatterInput)
      table2 <- table2 %>% filter(Batter %in% input$BatterInput)
    }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitcherThrows %in% splitinput, TaggedPitchType %in% pitchinput, PlayResult != "Undefined", Counts %in% countinput) %>% 
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('PA' = n(),
                       'BBE' = sum(PitchCall == "InPlay"),
                       'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                       'Max. EV' = round(max(ExitSpeed, na.rm = TRUE),1),
                       'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                       'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/sum(PitchCall == "InPlay")*100, 1),
                       'Barrel %' = round(sum(Barrel, na.rm = TRUE)/sum(PitchCall == "InPlay")*100, 1),
                       'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay")*100, 1),
                       'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay")*100, 1),
                       'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay")*100, 1),
                       'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay")*100, 1),
                       'LF %' = round(sum(Bearing >= -60 & Bearing <= -20 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1),
                       'CF %' = round(sum(Bearing >= -20 & Bearing <= 20 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1),
                       'RF %' = round(sum(Bearing >= 20 & Bearing <= 60 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1)
      ) %>%
      ungroup()
    
    table2 <- table2 %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitcherThrows %in% splitinput, TaggedPitchType %in% pitchinput, PlayResult != "Undefined", Counts %in% countinput) %>%
      dplyr::summarize('Pitch' = "Total",
                       'PA' = n(),
                       'BBE' = sum(PitchCall == "InPlay"),
                       'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                       'Max. EV' = round(max(ExitSpeed, na.rm = TRUE),1),
                       'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                       'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/sum(PitchCall == "InPlay")*100, 1),
                       'Barrel %' = round(sum(Barrel, na.rm = TRUE)/sum(PitchCall == "InPlay")*100, 1),
                       'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay")*100, 1),
                       'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay")*100, 1),
                       'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay")*100, 1),
                       'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay")*100, 1),
                       'LF %' = round(sum(Bearing >= -60 & Bearing <= -20 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1),
                       'CF %' = round(sum(Bearing >= -20 & Bearing <= 20 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1),
                       'RF %' = round(sum(Bearing >= 20 & Bearing <= 60 & !is.na(Bearing)) / max(sum(Bearing >= -60 & Bearing <= 60 & !is.na(Bearing)), 1)*100, 1) 
      )
    table <- bind_rows(table, table2)
    
    aux <- nrow(table) - 1
    table$hiddenColumn <- 0
    table$hiddenColumn[aux] <- 1
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = c(0,ncol(table)))))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(3,8,10,15), `border-right` = "solid 1px") %>% 
      formatStyle(1:ncol(table), valueColumns = "hiddenColumn", `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatStyle('Barrel %',
                  backgroundColor = styleInterval(c(7, 9), c('lightcoral', 'white', 'lightgreen'))) %>%
      formatStyle('Avg. EV',
                  backgroundColor = styleInterval(c(85, 89), c('lightcoral', 'white', 'lightgreen'))) %>%
      formatStyle('Hard Hit %',
                  backgroundColor = styleInterval(c(38, 42), c('lightcoral', 'white', 'lightgreen')))
    
  })
  
  ###############################################################################
  
  output$hit_location_plot <- renderPlot({
    table <- game
    
    if(input$PitcherTeamInput != "All") {
      table <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
    }
    
    if(input$BatterTeamInput != "All") {
      table <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
    }
    
    if(any(input$PitchInput == "All")){
      pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
    }
    else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
      input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
      input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
      input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
      if(length(input1) == 1){
        input1 <- c("Fastball", "Sinker")
      }
      if(length(input2) == 1){
        input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
      }
      if(length(input3) == 1){
        input3 <- c("ChangeUp", "Splitter")
      }
      pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
      pitchinput <- c(pitchinput, input1, input2, input3)
    }
    else{
      pitchinput = input$PitchInput
    }
    
    if(any(input$CountInput == "All")){
      countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
    }
    else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
      input4 <- input$CountInput[input$CountInput %in% c("Even")] 
      input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
      input6 <- input$CountInput[input$CountInput %in% c("Behind")]
      if(length(input4) == 1){
        input4 <- c("0-0", "1-1", "2-2")
      }
      if(length(input5) == 1){
        input5 <- c("0-1", "0-2", "1-2")
      }
      if(length(input6) == 1){
        input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
      }
      countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
      countinput <- c(countinput, input4, input5, input6)
    }
    else{
      countinput = input$CountInput
    }
    if(input$SplitInput == "Both"){
      splitinput = c("Right", "Left")
    }
    else{
      splitinput = input$SplitInput
    }
    if(input$BatterInput != "All") {
      table <- table %>% filter(Batter %in% input$BatterInput)
    }
    
    dataFilter <- reactive({
      table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitcherThrows %in% splitinput, TaggedPitchType %in% pitchinput, PitchCall == "InPlay", Counts %in% countinput)
    })
    
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location") +
      scale_color_manual(values = pitch_colors) +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") + # maybe linewidth instead of size
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), linewidth = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") + 
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") +
      geom_point(size = 3, na.rm = TRUE, alpha = 0.7) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  }, width = 350, height = 450)
  
  ###############################################################################
  
  output$spray_chart <- renderPlot({
    table <- game
    
    if(input$PitcherTeamInput != "All") {
      table <- table %>% filter(PitcherTeam %in% input$PitcherTeamInput)
    }
    
    if(input$BatterTeamInput != "All") {
      table <- table %>% filter(BatterTeam %in% input$BatterTeamInput)
    }
    
    if(any(input$PitchInput == "All")){
      pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
    }
    else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
      input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
      input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
      input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
      if(length(input1) == 1){
        input1 <- c("Fastball", "Sinker")
      }
      if(length(input2) == 1){
        input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
      }
      if(length(input3) == 1){
        input3 <- c("ChangeUp", "Splitter")
      }
      pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
      pitchinput <- c(pitchinput, input1, input2, input3)
    }
    else{
      pitchinput = input$PitchInput
    }
    
    if(any(input$CountInput == "All")){
      countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
    }
    else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
      input4 <- input$CountInput[input$CountInput %in% c("Even")] 
      input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
      input6 <- input$CountInput[input$CountInput %in% c("Behind")]
      if(length(input4) == 1){
        input4 <- c("0-0", "1-1", "2-2")
      }
      if(length(input5) == 1){
        input5 <- c("0-1", "0-2", "1-2")
      }
      if(length(input6) == 1){
        input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
      }
      countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
      countinput <- c(countinput, input4, input5, input6)
    }
    else{
      countinput = input$CountInput
    }
    if(input$SplitInput == "Both"){
      splitinput = c("Right", "Left")
    }
    else{
      splitinput = input$SplitInput
    }
    if(input$BatterInput != "All") {
      table <- table %>% filter(Batter %in% input$BatterInput)
    }
    
    dataFilter <- reactive({
      table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, PitcherThrows %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput) %>% 
        select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
    })
    
    geom_baseball(league = "MLB") +
      geom_point(data = dataFilter(), aes(round(Distance * sin(Bearing * pi / 180), 3), round(Distance * cos(Bearing * pi / 180), 3),
                                          color = ExitSpeed)) +
      scale_color_gradient(low = 'blue', high = 'red') +
      labs(title = "Spray Chart", x = "", y = "") + 
      #theme_bw() + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text = element_blank(),
            legend.position = "right",
            legend.text = element_text(size = 12))
    
  }, width = 450, height = 450)
  
}

shinyApp(ui = ui, server = server)
