#Clean the complete survey data
#This uses the direct output of the Intellisurvey data

### Survey cleaning

#Input own working directory
## verwijderd privacy

library("readxl")
library("reshape")
library("dplyr")
library("randomForest")
library("pdp")
library("vip")
library("tidyverse")
library("purrr")
library("readr")

#Load board level survey data
board_member_df <- read_excel("Data/Ruwe vragenlijsten/Board.member_vCodedAnswers.xlsx")[-1, ] %>%
  map_df(~parse_guess(.))
board_member_df$QBOARDNUM <- as.numeric(board_member_df$QBOARDNUM )
board_member_df$bm_exists <- 1
#Remove the columns with non-relevant indicators such as which browser is used, the time that the survey took etc.
board_member_df <- board_member_df[,-c(1:11)]
board_member_df <- board_member_df[,-c(2:5)]
board_member_df <- board_member_df[,-c(320:348)]

#Load HR level survey data
hr_df <- read_excel("Data/Ruwe vragenlijsten/HR_vCodedAnswers.xlsx")[-1, ] %>%
  map_df(~parse_guess(.))
hr_df$QBOARDNUM <- as.numeric(hr_df$QBOARDNUM )
hr_df$hr_exists <- 1
#Remove the columns with non-relevant indicators such as which browser is used, the time that the survey took etc.
hr_df <- hr_df[,-c(1:9)]
hr_df$QSCHOOLNUM <- NULL
hr_df <- hr_df[,-c(65:88)]

#Load Finance level survey data
finance_df <- read_excel("Data/Ruwe vragenlijsten/Finance_vCodedAnswers.xlsx")[-1, ] %>%
  map_df(~parse_guess(.))
finance_df$QBOARDNUM <- as.numeric(finance_df$QBOARDNUM )
finance_df$finance_exists <- 1
#Remove the columns with non-relevant indicators such as which browser is used, the time that the survey took etc.
finance_df <- finance_df[,-c(1:9)]
finance_df$QSCHOOLNUM <- NULL
finance_df <- finance_df[,-c(95:118)]

#Load Director level survey data
director_df <- read_excel("Data/Ruwe vragenlijsten/Director_vCodedAnswers.xlsx")[-1, ] %>%
  map_df(~parse_guess(.))
director_df$director_exists <- 1
director_df$BRIN6 <- str_sub(director_df$QD11X1,-6,-1)
director_df$QBOARDNUM <- str_sub(director_df$QD11X1,1,5)

#Correct BRIN6 in the director survey: for some schools the respondent did not put in a number, so we put it in manually via the Excel-file
CorrectedBRIN6Directors <- read_excel("Data/191104 Missing Schools Template.xlsx",sheet = "Director",skip = 1)
for(ID in CorrectedBRIN6Directors$`Record ID`[!is.na(CorrectedBRIN6Directors$`Opgezochte BRIN6`)]){
  director_df[director_df$id == ID,"BRIN6"] <- CorrectedBRIN6Directors$`Opgezochte BRIN6`[CorrectedBRIN6Directors$`Record ID` == ID]
  director_df[director_df$id == ID,"QBOARDNUM"] <- CorrectedBRIN6Directors$`Lookup BVG`[CorrectedBRIN6Directors$`Record ID` == ID]
}
#Remove the columns with non-relevant indicators such as which browser is used, the time that the survey took etc.
director_df <- director_df[,-c(1:24)]
director_df <- director_df[,-c(370:395)]

#Load Teacher level survey data
teacher_df  <- read_excel("Data/Ruwe vragenlijsten/Teacher_vCodedAnswers.xlsx")[-1, ] %>%
  map_df(~parse_guess(.))
teacher_df$teacher_exists <- 1
teacher_df$BRIN6 <- str_sub(teacher_df$QSCHOOLNUM_BRIN6,-6,-1)
teacher_df$QBOARDNUM <- str_sub(teacher_df$QSCHOOLNUM_BRIN6,1,5)

#Correct BRIN6 in the teacher survey: for some schools the respondent did not put in a number, so we put it in manually via the Excel-file
CorrectedBRIN6Teachers <- read_excel("Data/191104 Missing Schools Template.xlsx",sheet = "Teacher",skip = 1)
for(ID in CorrectedBRIN6Teachers$`Record ID`[!is.na(CorrectedBRIN6Teachers$`Opgezochte BRIN6`)]){
  teacher_df[teacher_df$id == ID,"BRIN6"] <- CorrectedBRIN6Teachers$`Opgezochte BRIN6`[CorrectedBRIN6Teachers$`Record ID` == ID]
  teacher_df[teacher_df$id == ID,"QBOARDNUM"] <- CorrectedBRIN6Teachers$`Lookup BVG`[CorrectedBRIN6Teachers$`Record ID` == ID]
}
#Remove the columns with non-relevant indicators such as which browser is used, the time that the survey took etc.
teacher_df <- teacher_df[,-c(1:14)]
teacher_df <- teacher_df[,-c(252:275)]

## Combine all surveys
#Combine at board level
survey <- board_member_df
survey <- merge(survey, hr_df, by="QBOARDNUM", all = TRUE)
survey <- merge(survey, finance_df, by="QBOARDNUM", all = TRUE)
colnames(survey)[which(names(survey) == "QBOARDNUM")] <- "BEVOEGD.GEZAG.NUMMER"

#Combine at BRIN6-level (individual schools)
survey_BRIN6 <- director_df
survey_BRIN6 <- merge(survey_BRIN6, teacher_df, by="BRIN6", all = TRUE)
survey_BRIN6$BEVOEGD.GEZAG.NUMMER <- ifelse(!is.na(survey_BRIN6$QBOARDNUM.x),survey_BRIN6$QBOARDNUM.x,
                       ifelse(!is.na(survey_BRIN6$QBOARDNUM.y), survey_BRIN6$QBOARDNUM.y,
                              NA))

#Remove the duplicates of which the BRIN6 was not correctly filled in (due to correction in either director or teacher survey)
for(BRIN6 in CorrectedBRIN6Teachers$`Opgezochte BRIN6`[!is.na(CorrectedBRIN6Teachers$`Opgezochte BRIN6`)]){
  survey_BRIN6[survey_BRIN6$BRIN6 == BRIN6,"BEVOEGD.GEZAG.NUMMER"] <- unique(survey_BRIN6$BEVOEGD.GEZAG.NUMMER[survey_BRIN6$BRIN6 == BRIN6 & survey_BRIN6$BEVOEGD.GEZAG.NUMMER != 997])
}
survey_BRIN6$QBOARDNUM.x<-NULL
survey_BRIN6$QBOARDNUM.y<-NULL
table(is.na(survey_BRIN6$BEVOEGD.GEZAG.NUMMER))

## Keep trackers
tracker <- survey_BRIN6[,c("BRIN6", "director_exists", "teacher_exists")]
tracker <- distinct(tracker, BRIN6,director_exists,teacher_exists)
length(unique(tracker$BRIN6))

tracker_bg <- survey[,c("BEVOEGD.GEZAG.NUMMER", "bm_exists", "finance_exists", "hr_exists")]
tracker_bg <- distinct(tracker_bg, BEVOEGD.GEZAG.NUMMER,bm_exists, finance_exists, hr_exists)
length(unique(tracker_bg$BEVOEGD.GEZAG.NUMMER))

### The next section selects the questions that we save eventually to the dataframe, all the relevant ones, to save some space
LegendQuestionsBM <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Board member")))
colnames(LegendQuestionsBM) <- "Vraagtekst"
LegendQuestionsFinance <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Finance")))
colnames(LegendQuestionsFinance) <- "Vraagtekst"
LegendQuestionsHR <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "HR")))
colnames(LegendQuestionsHR) <- "Vraagtekst"
LegendQuestions <- rbind(LegendQuestionsBM,LegendQuestionsFinance,LegendQuestionsHR)
LegendQuestions$Vraag <- as.character(rownames(LegendQuestions))
colnames(LegendQuestions) <- c("Vraagtekst","Category","Vraag")
#Also include the questions for the slides with general survey insights
QuestionsSurvey <- data.frame(t(read.csv("Data/VragenDashboardSlidesAlgemeen.csv",header = FALSE)))
ColumnstoExplore <- LegendQuestions$Vraag[(!is.na(LegendQuestions$Category)) | LegendQuestions$Vraag %in% QuestionsSurvey$X1]
AdditionalCategories <- c("BEVOEGD.GEZAG.NUMMER","BRIN6")

#Remove the numerical answers that should be NAs (answers like 'do not know' etc.)
survey_questions <- survey[,colnames(survey) %in% c(AdditionalCategories,ColumnstoExplore)]
survey_questions[survey_questions==99] <- NA
survey_questions[survey_questions==98] <- NA
survey_questions[survey_questions==97] <- NA

#Take the means per board, as for some schools multiple people have filled in the board survey
AverageAnswers <- matrix(, nrow = length(unique(survey_questions$BEVOEGD.GEZAG.NUMMER)), ncol = length(ColumnstoExplore))
#Use a for loop to take the average for every question over the respondents for that board (some have 2 respondents per board)
i = 1
for(BG in unique(survey_questions$BEVOEGD.GEZAG.NUMMER)){
  j = 1
  for(colname in ColumnstoExplore){
    AverageAnswers[i,j] <- round(mean(survey_questions[survey_questions$BEVOEGD.GEZAG.NUMMER == BG,colname],na.rm = TRUE))
    j = j+1
  }
  i = i+1
}
survey_means <- as.data.frame(unique(survey_questions[,c("BEVOEGD.GEZAG.NUMMER")]))
survey_means <- cbind(survey_means,AverageAnswers)
colnames(survey_means) <- c("BEVOEGD.GEZAG.NUMMER",ColumnstoExplore)
survey_means <- merge(survey_means, tracker_bg, by="BEVOEGD.GEZAG.NUMMER", all.x = TRUE, all.y = FALSE)

#Do the same on BRIN6-level
#Select the questions that we can use for analysis:
LegendQuestionsDirector <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Director")))
colnames(LegendQuestionsDirector) <- "Vraagtekst"
LegendQuestionsTeacher <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Teacher")))
colnames(LegendQuestionsTeacher) <- "Vraagtekst"
LegendQuestionsBRIN6 <- rbind(LegendQuestionsDirector,LegendQuestionsTeacher)
LegendQuestionsBRIN6$Vraag <- as.character(rownames(LegendQuestionsBRIN6))
colnames(LegendQuestionsBRIN6) <- c("Vraagtekst","Category","Vraag")
ColumnstoExploreBRIN6 <- LegendQuestionsBRIN6$Vraag[!is.na(LegendQuestionsBRIN6$Category) | LegendQuestionsBRIN6$Vraag %in% unique(QuestionsSurvey$X1)]
#Remove the numerical answers that should be NAs ('do not know' etc.)
survey_questionsBRIN6 <- survey_BRIN6[,colnames(survey_BRIN6) %in% c(AdditionalCategories,ColumnstoExploreBRIN6)]
survey_questionsBRIN6[survey_questionsBRIN6==99] <- NA
survey_questionsBRIN6[survey_questionsBRIN6==98] <- NA
survey_questionsBRIN6[survey_questionsBRIN6==97] <- NA
for(question in c("QD81R1","QD81R2","QD81R3","QD81R4","QD81R5","QD81R6","QD81R7","QD81R8","QD81R9","QD81R10")){
  survey_questionsBRIN6[survey_questionsBRIN6[[question]] == 4 & !is.na(survey_questionsBRIN6[[question]]),question] <- NA
}

#Take the means per BRIN6, as for some schools multiple people have filled in the director or teacher survey
AverageAnswers <- matrix(, nrow = length(unique(survey_questionsBRIN6$BRIN6)), ncol = length(ColumnstoExploreBRIN6))
#Use a for loop to take the average for every question over the respondents for that board (some have 2 respondents per BRIN6, e.g. 2 teachers)
i = 1
for(BRIN6 in unique(survey_questionsBRIN6$BRIN6)){
  j = 1
  for(colname in ColumnstoExploreBRIN6){
    AverageAnswers[i,j] <- round(mean(survey_questionsBRIN6[survey_questionsBRIN6$BRIN6 == BRIN6,colname],na.rm = TRUE))
    j = j+1
  }
  i = i+1
}
survey_BRIN6_means <- as.data.frame(unique(survey_questionsBRIN6[,c(AdditionalCategories)]))
survey_BRIN6_means <- cbind(survey_BRIN6_means,AverageAnswers)
colnames(survey_BRIN6_means) <- c(AdditionalCategories,ColumnstoExploreBRIN6)
survey_BRIN6_means$BEVOEGD.GEZAG.NUMMER <- NULL
survey_BRIN6_means <- merge(survey_BRIN6_means, tracker, by="BRIN6", all.x = TRUE, all.y = FALSE)

#Write everything to CSV
# write.csv(survey_means, "survey_BG.csv")
# write.csv(survey_BRIN6_means, file = "survey_BRIN6.csv")
