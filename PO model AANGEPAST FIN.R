#This script loads all the files needed for the analysis of BAO and makes it one big dataframe with columns on BRIN6-level

####PREPARE R AND LOAD PACKAGES####
#Set the working directory to own folder
## verwijderd privacy

#Load required packages, if this gives errors you might have to first install these
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
library("ranger")
library(testthat)

#Read the file with all PO data from the CLEANING file
data_PO_totaal <- read_excel("data/data_PO_all.xlsx") %>% arrange(BRIN6)
#data_PO <- data_PO[,-c(1)] #Drop the row number
data_PO_backup <- data_PO
data_PO <- data_PO %>% arrange(BRIN6)

assertthat::are_equal(all(data_PO$BRIN6 == data_PO_totaal$BRIN6), T)

data_PO <- cbind(data_PO[-(164:165)], data_PO_totaal[164:831]) %>% select(-percentiel, -top_bottom)

############################################################################
######## 1) Initial model of all nationally available data (no survey) #####
############################################################################

# Drop survey questions
data_PO_noQ <- data_PO[,-c(first(grep("QA46AC1.1", colnames(data_PO), fixed = TRUE)):grep("QE101R9", colnames(data_PO)))]
#Keep only the schools for which we have the required data, fill in missing data (NA) with 9999 so
#the random forest algorithm can use it in the decision trees
data_PO_FULL_ALL <- data_PO_noQ[complete.cases(data_PO_noQ$StdScore),]
data_PO_FULL_ALL <- data_PO_FULL_ALL[complete.cases(data_PO_FULL_ALL$StdSchoolweging),]
data_PO_FULL_ALL$percentiel <- NULL
data_PO_FULL_ALL$top_bottom <- NULL
data_PO_FULL_ALL[is.na(data_PO_FULL_ALL)] <- 9999
    # replace missing data so it can be used in RF model
data_PO_FULL <- data_PO_FULL_ALL[, -c(1,2,3,4,5,6,10,11,12)]
data_PO_FULL[sapply(data_PO_FULL, simplify = 'matrix', is.infinite)] <- 9999
    # replace infinite data so it can be used in RF model

#Run random forest model - baseline model
RandomForest_nosurvey <- randomForest(StdScore ~ .,
                       data=data_PO_FULL,
                       ntrees =1000,
                       localImp=TRUE,
                 seed=12345)

#Generate the results of the random forest model
RandomForest_nosurvey
vip(RandomForest_nosurvey,bar=TRUE,horizontal=TRUE,size=1.5)
max(RandomForest_nosurvey$rsq) #The R-squared found

#Normalize the variable importance to 100%
VariableImportances <- data.frame(randomForest::importance(RandomForest_nosurvey))
VariableImportances <- tibble::rownames_to_column(VariableImportances, "Variabele")
VariableImportances$VIPnormalized <- VariableImportances$X.IncMSE/sum(VariableImportances$X.IncMSE)
VariableImportances$VIPnormalizedFromR2 <- VariableImportances$VIPnormalized*max(RandomForest_nosurvey$rsq)
# write.csv(VariableImportances, "baseline_output.csv")
# this dataset is used to assign variables to buckets

###########################################################
#######2) Extend baseline model by including survey #######
###########################################################

# Select all relevant questions on bestuurslevel
data_PO_bestuur <- data_PO[,c(2,first(grep("QA46AC1.1", colnames(data_PO), fixed = TRUE)):grep("QA91R10", colnames(data_PO)))] #Select all questions
#data_PO_bestuur <- cbind(data_PO_bestuur$BRIN6, data_PO_bestuur[,-which(sapply(data_PO_bestuur, class) == "factor")]) #Make it one dataframe
#colnames(data_PO_bestuur)[1] <- "BRIN6"
data_PO_bestuur <- merge(data_PO_bestuur, data_PO[,c("BRIN6", "bm_exists")],by="BRIN6")
data_PO_bestuur <- subset(data_PO_bestuur, data_PO_bestuur$bm_exists==1) #Only take the schools for which we have bestuursvragenlijst
table(is.na(data_PO_bestuur))


#Clean up the dataframe and merge with the other data
data_PO_bestuur$bm_exists <- NULL
data_PO_bestuur[is.na(data_PO_bestuur)] <- 9999
data_PO_bestuur <- base::merge(data_PO_FULL_ALL, data_PO_bestuur, by="BRIN6", all.x=FALSE, all.y=FALSE)
data_PO_bestuur[sapply(data_PO_bestuur, simplify = 'matrix', is.infinite)] <- 9999


#############################
## TOEVOEGING FINANCIEN
fake_vars_bestuur <- cbind(unique(data_PO_FULL_ALL$BEVOEGD.GEZAG.NUMMER), data.frame(sapply(1:200, function(x) round(runif(831, 0, 6)))))
colnames(fake_vars_bestuur)[1] = 'BEVOEGD.GEZAG.NUMMER'
data_PO_bestuur <- data_PO_bestuur %>% left_join(fake_vars_bestuur, by = 'BEVOEGD.GEZAG.NUMMER')
#############################


### Remove survey questions that do not make sense for the model
survey_to_drop <- read_excel("data/selectie_survey/Categorisering_model_PO.xlsx", sheet = "Categorisering")

survey_dropped <- subset(survey_to_drop, Categorie=="OUT")
survey_dropped <- as.vector(survey_dropped$Variabele)
data_PO_bestuur <- data_PO_bestuur[,!names(data_PO_bestuur)%in%survey_dropped]
data_PO_bestuur$director_exists<-NULL
data_PO_bestuur$teacher_exists<-NULL
table(survey_to_drop$Categorie)

data_PO_bestuur_all <- data_PO_bestuur
data_PO_bestuur <- data_PO_bestuur[, -c(1,2,3,4,5,6,10,11,12)] #Remove all the identifier columns (BRIN, gemeentenaam etc)

#Run the random forest model for the data including the survey on board level
RandomForest_surveyboard <- randomForest(StdScore ~ .,
                       data=data_PO_bestuur,
                       ntrees =1000,
                       localImp=TRUE,
                       seed=12345)

#Results
RandomForest_surveyboard
scatter.smooth(RandomForest_surveyboard$predicted, data_PO_bestuur$StdScore)
vip(RandomForest_surveyboard,bar=TRUE,horizontal=TRUE,size=1.5)

#Normalize the variable importance to 100%
VariableImportances_board <- data.frame(randomForest::importance(RandomForest_surveyboard))
VariableImportances_board$VIPnormalized <- VariableImportances_board$X.IncMSE/sum(VariableImportances$X.IncMSE)
VariableImportances_board$VIPnormalizedFromR2 <- VariableImportances_board$VIPnormalized*max(RandomForest_surveyboard$rsq)

# Merge subcategory to each VIP
output <- VariableImportances_board
output <- tibble::rownames_to_column(output, "Variabele")
output <- merge(output, survey_to_drop[,c("Variabele","Sub_Categorie","Categorie")], by="Variabele", all.x = T)

output$Categorie[is.na(output$Categorie)] = 'X'
output$Sub_Categorie[output$Categorie == 'X'] = c('A', 'B', 'C', 'D', 'E')

OutDrop <- output$Variabele[output$Categorie=="Out"]



### Variable selection and rerun
#Based on the VIP determine the variables to be selected for each factor
output <- output %>%
  group_by(Categorie,Sub_Categorie) %>%
  mutate(rank = order(order(VIPnormalizedFromR2, decreasing=TRUE)))
output <- subset(output, rank>10)
output_to_drop <- as.vector(unique(c(output$Variabele,OutDrop)))

data_PO_bestuur100 <- data_PO_bestuur[,!names(data_PO_bestuur)%in%output_to_drop]

RandomForest_surveyboard100 <- randomForest(StdScore ~ .,
                                  data=data_PO_bestuur100,
                                  ntrees =1000,
                                  localImp=TRUE,
                                  seed=12345)
RandomForest_surveyboard100
scatter.smooth(RandomForest_surveyboard100$predicted, data_PO_bestuur100$StdScore)
vip(RandomForest_surveyboard100,bar=TRUE,horizontal=TRUE,size=1.5)
max(RandomForest_surveyboard100$rsq)
#Normalize the variable importance to 100%
VariableImportances_board100 <- data.frame(randomForest::importance(RandomForest_surveyboard100))
VariableImportances_board100$VIPnormalized <- VariableImportances_board100$X.IncMSE/sum(VariableImportances_board100$X.IncMSE)
VariableImportances_board100$VIPnormalizedFromR2 <- VariableImportances_board100$VIPnormalized*max(RandomForest_surveyboard100$rsq)
# write.csv(VariableImportances_board100, "VariableImportances_board100.csv")
# this dataset is used to assign variables to buckets

output <- VariableImportances_board100
output <- tibble::rownames_to_column(output, "Variabele")
output <- merge(output, survey_to_drop[,c("Variabele","Sub_Categorie","Categorie")], by="Variabele", all.x = T)

output$Categorie[is.na(output$Categorie)] = 'X'

output %>% group_by(Categorie) %>% summarise(importance = sum(VIPnormalized), n = n()) %>% mutate(imp_per_n = importance / n)

output %>% filter(startsWith(Variabele, 'X')) %>% summarise(sum(VIPnormalized))
output %>% filter(startsWith(Variabele, 'Q')) %>% summarise(sum(VIPnormalized))

