#Coach Scully Report: Working Ahead, Command, Efficiency, Make-Up


#Load Libraries
library(dplyr)
library(gridExtra)
library(gridGraphics)
library(ggplot2)
library(patchwork)

#Load Data
data2023 <- read.csv("20240317-McKieFieldStad-1_unverified.csv")

#Select the player and game date
data20231 <- filter(data2023, Pitcher == "Vardavas, Nick")

#Only use columns we need
data20232 <- subset(data20231, select = c(PitchNo, PAofInning, PitchofPA, Date, Pitcher, PitcherId,
                                          Inning, Outs, Balls, Strikes, TaggedPitchType,
                                          PitchCall, PlayResult, KorBB, RunsScored))
data20232 <- data20232 %>%
  mutate(paofinningupdate = paste0(Date, Inning, PAofInning))



#---------------------WORKING AHEAD--------------------


# ----------- 0-1 counts that become 0-2 ------------

data20232$Count01 <- ifelse(data20232$Ball == "0" & data20232$Strike == "1" , 1, 0)
# Find row indices where Count01 is 1
indices <- which(data20232$Count01 == 1)

# Create an empty list to store filtered rows
filtered_rows <- list()

# Loop through the indices and keep current row and the subsequent row
for (i in indices) {
  if (i < nrow(data20232)) {
    filtered_rows[[length(filtered_rows) + 1]] <- data20232[i:(i+1), ]
  }
}

# Combine the list of data frames into a single data frame
final_01 <- do.call(rbind, filtered_rows)

# Remove duplicate rows if any
final_01 <- unique(final_01)

#Remove columns of a fresh AB
final_01 <- final_01 %>%
  filter(!(Balls == 0 & Strikes == 0))

#Remove the 0-1 count columns to such leave us with the subsequent pitches
final_01 <- filter(final_01, Count01 != 1)

#Create binary for counts that became 0-2
final_01$Count02 <- ifelse(final_01$Ball == "0" & final_01$Strike == "2" , 1, 0)

final_01 <- subset(final_01, select = c(PitchNo, PitcherId, Pitcher, Count02))

final_01 <- final_01 %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`% 0-1 Counts that become 0-2` = mean(Count02, na.rm = TRUE),
            Pitches_Considered = n())

final_01$`% 0-1 Counts that become 0-2` <- round(final_01$`% 0-1 Counts that become 0-2` * 100, 2)
final_01$`% 0-1 Counts that become 0-2` <- paste(final_01$`% 0-1 Counts that become 0-2`, "%", sep = "")

# ----------- 1-1 counts that become 1-2 ------------

data20232$Count11 <- ifelse(data20232$Ball == "1" & data20232$Strike == "1" , 1, 0)
# Find row indices where Count01 is 1
indices1 <- which(data20232$Count11 == 1)

# Create an empty list to store filtered rows
filtered_rows1 <- list()

# Loop through the indices and keep current row and the subsequent row
for (i in indices1) {
  if (i < nrow(data20232)) {
    filtered_rows1[[length(filtered_rows1) + 1]] <- data20232[i:(i+1), ]
  }
}

# Combine the list of data frames into a single data frame
final_11 <- do.call(rbind, filtered_rows1)

# Remove duplicate rows if any
final_11 <- unique(final_11)

#Remove columns of a fresh AB
final_11 <- final_11 %>%
  filter(!(Balls == 0 & Strikes == 0))

#Remove the 0-1 count columns to such leave us with the subsequent pitches
final_11 <- filter(final_11, Count11 != 1)

#Create binary for counts that became 1-2
final_11$Count12 <- ifelse(final_11$Ball == "1" & final_11$Strike == "2" , 1, 0)

final_11 <- subset(final_11, select = c(PitchNo, PitcherId, Pitcher, Count12))

final_11 <- final_11 %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`% 1-1 Counts that become 1-2` = mean(Count12, na.rm = TRUE),
            Pitches_Considered = n())

final_11$`% 1-1 Counts that become 1-2` <- round(final_11$`% 1-1 Counts that become 1-2` * 100, 2)
final_11$`% 1-1 Counts that become 1-2` <- paste(final_11$`% 1-1 Counts that become 1-2`, "%", sep = "")

#-------------First Pitch Strike------------
data20232_FirstPitchK <- data20232 %>%
  filter(PitchofPA == 1)

data20232_FirstPitchK$FirstPitchK <- ifelse(data20232_FirstPitchK$PitchCall == "StrikeCalled" | 
                                            data20232_FirstPitchK$PitchCall == "StrikeSwinging" |
                                            data20232_FirstPitchK$PitchCall == "FoulBall" |
                                            data20232_FirstPitchK$PitchCall == "FoulBallNotFieldable" | 
                                            data20232_FirstPitchK$PitchCall == "InPlay", 1, 0)

data20232_FirstPitchK <- subset(data20232_FirstPitchK, select = c(PitchNo, PitcherId, Pitcher, FirstPitchK))

data20232_FirstPitchK <- data20232_FirstPitchK %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`1st Pitch Strike %` = mean(FirstPitchK, na.rm = TRUE),
            Pitches_Considered = n())

data20232_FirstPitchK$`1st Pitch Strike %` <- round(data20232_FirstPitchK$`1st Pitch Strike %` * 100, 2)
data20232_FirstPitchK$`1st Pitch Strike %` <- paste(data20232_FirstPitchK$`1st Pitch Strike %`, "%", sep = "")

#First 2 of 3 Pitches Strikes

FirstTwoOfThree <- data20232 %>%
  group_by(paofinningupdate) %>%
  mutate(count = n()) %>%
  filter(count >= 3) %>%
  slice_head(n = 3) %>%
  select(-count) %>%
  ungroup()


FirstTwoOfThree <- FirstTwoOfThree %>%
  group_by(paofinningupdate) %>%
  mutate(
    `FirstTwoOfThreeStrike%` = as.integer(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay", "FoulBallNotFieldable")) >= 2)
  )

FirstTwoOfThree <- FirstTwoOfThree %>%
  group_by(PitcherId, Pitcher, paofinningupdate) %>%
  summarize(`FirstTwoOfThreeStrike%` = mean(`FirstTwoOfThreeStrike%`, na.rm = TRUE))

FirstTwoOfThree <- FirstTwoOfThree %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`FirstTwoOfThreeStrike%` = mean(`FirstTwoOfThreeStrike%`, na.rm = TRUE),
            PA_Considered = n())

FirstTwoOfThree$`FirstTwoOfThreeStrike%` <- round(FirstTwoOfThree$`FirstTwoOfThreeStrike%` * 100, 2)
FirstTwoOfThree$`FirstTwoOfThreeStrike%` <- paste(FirstTwoOfThree$`FirstTwoOfThreeStrike%`, "%", sep = "")


#-----------------------COMMAND-----------------------
strike_vector <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay", "FoulBallNotFieldable")

Command <- data20231 %>%
  group_by(Pitcher, PitcherId, TaggedPitchType) %>%
  mutate(strike = if_else(PitchCall %in% strike_vector, 1, 0)) %>%
  summarize(`Strike %` = sum((strike)/n()),
            Pitches_Considered = n())

Command$`Strike %` <- round(Command$`Strike %` * 100, 2)
Command$`Strike %` <- paste(Command$`Strike %`, "%", sep = "")

Command <- Command %>%
  arrange(desc(Pitches_Considered))

#----------------------EFFICIENCY---------------------
# Calculate Ball3Counts
#Percent of PA that go to 3 ball Counts
Ball3Counts <- data20232 %>%
  group_by(paofinningupdate) %>%
  mutate(
    `% of PA's that go to 3 Balls Count` = as.integer(any(Balls == 3))
  )

Ball3Counts <- Ball3Counts %>%
  group_by(PitcherId, Pitcher, paofinningupdate) %>%
  summarize(`% of PA's that go to 3 Balls Count` = mean(`% of PA's that go to 3 Balls Count`, na.rm = TRUE))

Ball3Counts <- Ball3Counts %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`% of PA's that go to 3 Balls Count` = mean(`% of PA's that go to 3 Balls Count`, na.rm = TRUE),
            PA_Considered = n())

Ball3Counts$`% of PA's that go to 3 Balls Count` <- round(Ball3Counts$`% of PA's that go to 3 Balls Count` * 100, 2)
Ball3Counts$`% of PA's that go to 3 Balls Count` <- paste(Ball3Counts$`% of PA's that go to 3 Balls Count`, "%", sep = "")


#First Batter of Inning Out %
FirstBatter <- data20232 %>%
  filter(substr(paofinningupdate, nchar(paofinningupdate), nchar(paofinningupdate)) == "1")

FirstBatterLastRow <- FirstBatter %>%
  group_by(paofinningupdate) %>%
  slice_tail(n = 1) %>%
  ungroup()

FirstBatterLastRow$Out <- if_else(FirstBatterLastRow$PlayResult == "Out" | FirstBatterLastRow$PitchCall == "StrikeSwinging" |
                                  FirstBatterLastRow$PitchCall == "StrikeCalled", 1, 0)

FirstBatterLastRow <- FirstBatterLastRow %>%
  group_by(PitcherId, Pitcher) %>%
  summarize(`First Batter Of Inning Out %` = mean(`Out`, na.rm = TRUE),
            PA_Considered = n())

FirstBatterLastRow$`First Batter Of Inning Out %` <- round(FirstBatterLastRow$`First Batter Of Inning Out %` * 100, 2)
FirstBatterLastRow$`First Batter Of Inning Out %` <- paste(FirstBatterLastRow$`First Batter Of Inning Out %`, "%", sep = "")

#3 Or Less Pitch PA
result <- data20232 %>%
  group_by(paofinningupdate, Pitcher, PitcherId) %>%
  summarize(count = n()) %>%
  mutate(ThreeorLess = ifelse(count <= 3, 1, 0))

ThreeOrLessPitchPA <- result %>%
  group_by(Pitcher, PitcherId) %>%
  summarize(`3 or Less Pitch PA %` = mean(ThreeorLess, na.rm = TRUE),
            PA_Considered = n())

ThreeOrLessPitchPA$`3 or Less Pitch PA %` <- round(ThreeOrLessPitchPA$`3 or Less Pitch PA %` * 100, 2)
ThreeOrLessPitchPA$`3 or Less Pitch PA %` <- paste(ThreeOrLessPitchPA$`3 or Less Pitch PA %`, "%", sep = "")

#4 Or Less Pitch PA
result1 <- data20232 %>%
  group_by(paofinningupdate, Pitcher, PitcherId) %>%
  summarize(count = n()) %>%
  mutate(FourorLess = ifelse(count <= 4, 1, 0))

FourOrLessPitchPA <- result1 %>%
  group_by(Pitcher, PitcherId) %>%
  summarize(`4 or Less Pitch PA %` = mean(FourorLess, na.rm = TRUE),
            PA_Considered = n())

FourOrLessPitchPA$`4 or Less Pitch PA %` <- round(FourOrLessPitchPA$`4 or Less Pitch PA %` * 100, 2)
FourOrLessPitchPA$`4 or Less Pitch PA %` <- paste(FourOrLessPitchPA$`4 or Less Pitch PA %`, "%", sep = "")


#--------------------MAKEUP--------------------

#Percent of Runners that Score
Perc_Runners_Who_Score <- data20231
Perc_Runners_Who_Score$Runners <- if_else(Perc_Runners_Who_Score$PlayResult == "Double" | 
                                            Perc_Runners_Who_Score$PlayResult == "Triple" |
                                            Perc_Runners_Who_Score$PlayResult == "HomeRun" |
                                            Perc_Runners_Who_Score$PlayResult == "Single" |
                                            Perc_Runners_Who_Score$PlayResult == "Error" |
                                            Perc_Runners_Who_Score$KorBB == "Walk"|
                                            Perc_Runners_Who_Score$PitchCall == "HitByPitch", 1, 0)

Perc_Runners_Who_Score1 <- subset(Perc_Runners_Who_Score, select = c(PitchNo, PAofInning, PitchofPA, Date, Pitcher, PitcherId,
                                                                     Inning, Outs, Balls, Strikes, TaggedPitchType,
                                                                     PitchCall, PlayResult, KorBB, RunsScored, Runners))

Perc_Runners_Who_Score2 <- Perc_Runners_Who_Score1 %>%
  group_by(Pitcher, PitcherId) %>%
  summarize(`% of Runners That Score` = sum(RunsScored, na.rm = TRUE) / sum(Runners, na.rm = TRUE),
            `Runners Considered` = sum(Runners == 1, na.rm = TRUE))

Perc_Runners_Who_Score2$`% of Runners That Score` <- round(Perc_Runners_Who_Score2$`% of Runners That Score` * 100, 2)
Perc_Runners_Who_Score2$`% of Runners That Score` <- paste(Perc_Runners_Who_Score2$`% of Runners That Score`, "%", sep = "")

#Percent of 2-0, 2-1, 3 Ball Counts ending in outs
BadCounts <- data20232 %>%
  group_by(paofinningupdate) %>%
  filter(
    (any(Balls == 2 & Strikes == 0) ||
       any(Balls == 2 & Strikes == 1) ||
       any(Balls == 3))
  ) %>%
  ungroup() %>%
  group_by(paofinningupdate) %>%
  slice_tail(n = 1)  

BadCounts$Out <- if_else(BadCounts$PlayResult == "Out" | BadCounts$KorBB == "Strikeout", 1, 0)

BadCounts <- BadCounts %>%
  group_by(Pitcher, PitcherId) %>%
  summarize(`% of 2-0, 2-1, 3 Ball Counts Ending In Outs` = mean(Out, na.rm = TRUE),
            PA_Considered = n())

BadCounts$`% of 2-0, 2-1, 3 Ball Counts Ending In Outs` <- round(BadCounts$`% of 2-0, 2-1, 3 Ball Counts Ending In Outs` * 100, 2)
BadCounts$`% of 2-0, 2-1, 3 Ball Counts Ending In Outs` <- paste(BadCounts$`% of 2-0, 2-1, 3 Ball Counts Ending In Outs`, "%", sep = "")

#------------------------Combine the Dataframes-----------------------------
#--------Working ahead--------
workingahead <- cbind(final_01, final_11, FirstTwoOfThree)
workingahead <- workingahead[,c(2:4, 7:8, 11:12)]


colnames(workingahead) <- c("Pitcher", "% 0-1 That Go 0-2", "Pitches_Cons1", 
                          "% 1-1 That Go 1-2", "Pitches_Cons2", 
                          "First_Two_Of_Three_Strike%", "PA_Cons1") 

# convert columns to numeric

# Create the tableGrob object
table <- tableGrob(workingahead, rows = NULL)

# Create a textGrob for the title
title <- textGrob("Nick Vardavas Working Ahead", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table <- grid.arrange(title, table, ncol = 1, heights = c(0.05, 0.95))

#--------Command---------
Command <- Command[,c(1, 3:5)]

table1 <- tableGrob(Command, rows = NULL)

# Create a textGrob for the title
title1 <- textGrob("Nick Vardavas Command", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table1 <- grid.arrange(title1, table1, ncol = 1, heights = c(0.05, 0.95))

#-------Efficiency-------
Efficiency <- cbind(Ball3Counts, FirstBatterLastRow, ThreeOrLessPitchPA, FourOrLessPitchPA)
Efficiency <- Efficiency[,c(2:4, 7:8, 11:12, 15:16)]

colnames(Efficiency) <- c("Pitcher", "% of PA's that go to 3 Balls Count", "Pitches_Cons1", 
                          "1st Batter of Inning Out %", "PA_Cons1", 
                          "3 Or Less Pitch PA %", "PA_Cons2", "4 Or Less Pitch PA %", "PA_Cons3") 


table2 <- tableGrob(Efficiency, rows = NULL)

# Create a textGrob for the title
title2 <- textGrob("Nick Vardavas Efficiency", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table2 <- grid.arrange(title2, table2, ncol = 1, heights = c(0.40, 0.70))


#-----Make-Up------
MakeUp <- cbind(Perc_Runners_Who_Score2, BadCounts)

MakeUp <- MakeUp[,c(1, 3:4, 7:8)]

colnames(MakeUp) <- c("Pitcher", "% of Runners That Score", "Runners_Cons", 
                          "% of 2-0, 2-1, 3-0 Counts Resulting in Outs", "PA_Cons1") 

table3 <- tableGrob(MakeUp, rows = NULL)

# Create a textGrob for the title
title3 <- textGrob("Nick Vardavas Makeup", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and tableGrob using grid.arrange
table3 <- grid.arrange(title3, table3, ncol = 1, heights = c(0.05, 0.95))


#----------Combine the Plots---------
plot_layout1 <- "
  A
  B
  C
  D"

second_graphic <- wrap_plots(table, table1, table2, table3,
                             design = plot_layout1)

ggsave("second_graphic.png", plot = second_graphic, width = 16, height = 8, units = "in")



