#This script allows a comparison of below and above average schools ('top and bottom') for the survey questions

#Set your own working directory
## verwijderd privacy

#Load required packages (install if this is first time)
library("readxl")
library("reshape")
library("dplyr")
library("randomForest")
library("pdp")
library("vip")
library("Hmisc")
library("Benchmarking")
library("swfscMisc")
library("stringr")
library("tidyverse")
library("scales")

#Input the parameters for analysis
RankPercentage <- 33 #Top schools are defined as above the (100-percentile filled in here); bottom schools as below the percentile filled in here
Consistency_Range <- 1 #Only for BAO/PO: The maximum difference between the max and min Value-added of the school to count as either top or bottom, measured in standard deviations
options(digits = 3) #3 digits rounding

#Load PO, VO data
data_PO <- read.csv("Data/Masterfiles/data_PO_all.csv")
data_PO <- data_PO[,-c(1)] #Remove row numbers
data_VO <- read.csv("Data/Masterfiles/data_VO_all.csv")
data_VO <- data_VO[,-c(1)] #Remove row numbers

#PO:
#Determine top and bottom based on consistent VA schools
Bottom <- quantile(data_PO$VA[data_PO$range <Consistency_Range],c(RankPercentage)/100,na.rm = TRUE)
Top <- quantile(data_PO$VA[data_PO$range <Consistency_Range],c(100-RankPercentage)/100,na.rm = TRUE)
data_PO$Rank <- ifelse(data_PO$VA >= Top &(data_PO$range<Consistency_Range),
                       "top",
                       ifelse(data_PO$VA <= Bottom &(data_PO$range<Consistency_Range),
                              "bottom",""))

#VO:
#Determine top and bottom based on the corrected Inspectiescore CE
Bottom <- quantile(data_VO$VA,c(RankPercentage)/100,na.rm = TRUE)
Top <- quantile(data_VO$VA,c(100-RankPercentage)/100,na.rm = TRUE)
data_VO$Rank <- ifelse(data_VO$VA >= Top,
                       "top",
                       ifelse(data_VO$VA <= Bottom,
                              "bottom",""))

#For all the survey questions, load the actual question formulation:
LegendQuestionsBM <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Board member")))
colnames(LegendQuestionsBM) <- "Vraagtekst"
LegendQuestionsFinance <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Finance")))
colnames(LegendQuestionsFinance) <- "Vraagtekst"
LegendQuestionsHR <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "HR")))
colnames(LegendQuestionsHR) <- "Vraagtekst"
LegendQuestionsDirector <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Director")))
colnames(LegendQuestionsDirector) <- "Vraagtekst"
LegendQuestionsTeacher <- data.frame(t(read_excel("Data/Selection.xlsx",sheet = "Teacher")))
colnames(LegendQuestionsTeacher) <- "Vraagtekst"
LegendQuestions <- rbind(LegendQuestionsBM,LegendQuestionsFinance,LegendQuestionsHR,LegendQuestionsDirector,LegendQuestionsTeacher)
LegendQuestions$Vraag <- as.character(rownames(LegendQuestions))
colnames(LegendQuestions) <- c("Vraagtekst","Category","Vraag")

#Select which questions to compare top and bottom for
Categorizering <- read.csv("Data/SelectedQuestions.csv")

ColumnstoExplore <- LegendQuestions$Vraag[!is.na(LegendQuestions$Category) | LegendQuestions$Vraag %in% Categorizering$Variabele]
AdditionalCategories <- c("BEVOEGD.GEZAG.NUMMER","BRIN6","BRIN6_per_BG","GroeiLeerlingenGemeente20162021","IMPULSGEBIED","VAadjusted","range","Rank")

######Combined tests for PO, VO #######
#Take the data for which we have value-added
data_PO <- data_PO[!is.na(data_PO$VA),]
data_VO <- data_VO[!is.na(data_VO$VA),]

######TOP-BOTTOM ANALYSE#######
#Initialize the dataframe that we need as output
Reset <- data.frame(Percentiel_ranking = double(),Consist_range = double(),
                    Vraag = character(),
                    Sector = character(),Segment = character(),
                    ObservatiesScholen = double(),AantalTopBottomScholen = double(),ObservatiesBesturen = double(),AantalTopBottomBesturen = double(),
                    Top = double(),Average = double(),Bottom = double(),Difference_Abs = double(),
                    Topsimple = double(),Bottomsimple = double(),Difference_Agreement = double(),
                    stringsAsFactors = FALSE)
Total <- Reset

Sectors <- c("PO","VO") #Select the sectors that should be analyzed

SegmentNames <- "All" #Analyses on segment level can also be made

for(Sector in Sectors){
  #Test all questions for directors and teachers per school, do a t-test on the difference between the top and the bottom group
  if(Sector == "PO"){
    dataset <- data_PO
  }
  if(Sector == "VO"){
    dataset <- data_VO
  }

  for(Segment in SegmentNames){
    SegmentRows <- dataset
    #Filter on the right segment
    if(Segment != "All"){
      SegmentRows <- dataset[grepl(Segment,paste(dataset$KrimpGroei,dataset$Bestuursgrootte,dataset$Impuls),fixed=TRUE),]
    }

    TopTest <- Reset
    i=1
    for(Vraag in ColumnstoExplore){
      #Determine top and bottom schools that have answered the question
      temp <- SegmentRows[!is.na(SegmentRows[[Vraag]]),c(Vraag,"Rank","BEVOEGD.GEZAG.NUMMER","VA")]
      Toprows <- temp[temp$Rank == "top" & !is.na(temp$Rank),]
      Bottomrows <- temp[temp$Rank == "bottom" & !is.na(temp$Rank),]
        if(nrow(Toprows)>5 & nrow(Bottomrows) > 5){
        #Determine the % the have indicated eens or zeer eens for the top-bottom analyses as presented in the factoren-slides
        tempsimple <- temp
        tempsimple[tempsimple[[Vraag]]<=4,Vraag]<-0 #Should only be done for statement questions
        tempsimple[tempsimple[[Vraag]]>4,Vraag]<-1 #Should only be done for statement questions
        Toprowssimple <- tempsimple[tempsimple$Rank == "top" & !is.na(tempsimple$Rank),]
        Bottomrowssimple <- tempsimple[tempsimple$Rank == "bottom" & !is.na(tempsimple$Rank),]

        #Determine the difference top and bottom scoring schools in terms of VA
        if(!identical(Toprows[[Vraag]],Bottomrows[[Vraag]])){
          TopTest[i,] <- c(RankPercentage,Consistency_Range,
                           Vraag,
                           Sector,Segment,
                           nrow(temp),nrow(temp[temp$Rank != "",]),length(unique(temp$BEVOEGD.GEZAG.NUMMER)),length(unique(temp[temp$Rank != "","BEVOEGD.GEZAG.NUMMER"])),
                           mean(Toprows[[Vraag]]),mean(temp[[Vraag]]),mean(Bottomrows[[Vraag]]),mean(Toprows[[Vraag]])-mean(Bottomrows[[Vraag]]),
                           mean(Toprowssimple[[Vraag]]),mean(Bottomrowssimple[[Vraag]]),mean(Toprowssimple[[Vraag]])-mean(Bottomrowssimple[[Vraag]]))
                           i = i+1
        }
      }
    }
    Total <- rbind(Total,TopTest)
  }
}

#Add description of the question to the dataframe
Total <- merge(Total,LegendQuestions[,-2],by="Vraag",all.x=TRUE)
