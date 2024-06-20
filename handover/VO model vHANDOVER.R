####PREPARE R AND LOAD PACKAGES####
## setwd verwijderd privacy

library("readxl")
library("reshape")
library(dplyr)
library("randomForest")
library("pdp")
library("vip")

## setwd verwijderd privacy

data_VO <- read.csv(paste(file="C:/Users/",Sys.info()[7], "data/data_VO_all.csv", sep = "",""))
data_VO <- data_VO[,-c(1)]

######## 1) Initial model #####
# Leave out survey questions
data_VO_noQ <- data_VO[,-c(first(grep("QA46AC1.1", colnames(data_VO), fixed = TRUE)):grep("QE101R9", colnames(data_VO)))]
table(is.na(data_VO_noQ))
data_VO_FULL_ALL <- data_VO_noQ[complete.cases(data_VO_noQ$StdCE),]
data_VO_FULL_ALL <- data_VO_FULL_ALL[complete.cases(data_VO_FULL_ALL$StdCITO),]

#Add contextfactoren inspectie data
inspectiefactoren <- read_excel("inspectie.xlsx", sheet = "Totaal overzicht",skip = 1)
inspectiefactoren <- inspectiefactoren[,colnames(inspectiefactoren)[!substr(colnames(inspectiefactoren),1,2) %in% c("na","ad","CE","To","SE")]]
colnames(inspectiefactoren) <- c("BRIN6","VMBOB_BB_succes","VMBOB_OB_snelheid","VMBOB_OB_advies","VMBOK_BB_succes","VMBOK_OB_snelheid","VMBOK_OB_advies","VMBOGT_BB_succes","VMBOGT_OB_snelheid","VMBOGT_OB_advies","HAVO_BB_succes","HAVO_OB_snelheid","HAVO_OB_advies","VWO_BB_succes","VWO_OB_snelheid","VWO_OB_advies")
data_VO_FULL_ALL <- merge(data_VO_FULL_ALL,inspectiefactoren,by="BRIN6",all.x=TRUE)

data_VO_FULL_ALL[is.na(data_VO_FULL_ALL)] <- 9999
      # relabel missings as 9999 so this information can be used in the model
data_VO_FULL <- data_VO_FULL_ALL[, -c(1,2,3,4,5,8,9,10,13,14)]
data_VO_FULL[sapply(data_VO_FULL, simplify = 'matrix', is.infinite)] <- 9999
      # relabel missings as 9999 so this information can be used in the model
data_VO_FULL <- data_VO_FULL[,colnames(data_VO_FULL)[!(substr(colnames(data_VO_FULL),1,8)) %in% c("CEtotaal","CEcorrec","CE_aanta")]]

ModelVO0 <- randomForest(StdCE ~ .,
                       data=data_VO_FULL,
                       ntree=1000,
                       localImp=TRUE)

ModelVO0
vip(ModelVO0,bar=TRUE,horizontal=TRUE,size=1.5)

######## 2) Model Bestuur survey####
data_VO_bestuur <- data_VO[,c(1,first(grep("QA46AC1.1", colnames(data_VO), fixed = TRUE)):grep("QA91R10", colnames(data_VO)))]
data_VO_bestuur <- cbind(data_VO_bestuur$BRIN6, data_VO_bestuur[,-which(sapply(data_VO_bestuur, class) == "factor")])
colnames(data_VO_bestuur)[1] <- "BRIN6"
data_VO_bestuur <- merge(data_VO_bestuur, data_VO[,c("BRIN6", "bm_exists")],by="BRIN6")
data_VO_bestuur <- subset(data_VO_bestuur, data_VO_bestuur$bm_exists==1)
data_VO_bestuur$bm_exists <- NULL
table(is.na(data_VO_bestuur))
data_VO_bestuur[is.na(data_VO_bestuur)] <- 9999
data_VO_bestuur <- base::merge(data_VO_FULL_ALL, data_VO_bestuur, by="BRIN6", all.x=FALSE, all.y=FALSE)

### Remove non-sensical survey questions
survey_to_dropVO <- read_excel("data/Categorisering_model_VO.xlsx", sheet = "Categorisering")
survey_droppedVO <- subset(survey_to_dropVO, Categorie=="OUT")
survey_droppedVO <- as.vector(survey_droppedVO$Variabele)
data_VO_bestuur <- data_VO_bestuur[,!names(data_VO_bestuur)%in%survey_droppedVO]

data_VO_bestuur$director_exists<-NULL
data_VO_bestuur$teacher_exists<-NULL
table(survey_to_dropVO$Categorie)

data_VO_bestuur <- data_VO_bestuur[, -c(1,2,3,4,5,8,9,10,13:25)]
data_VO_bestuur[sapply(data_VO_bestuur, simplify = 'matrix', is.infinite)] <- 99999
# relabel missings as 9999 so this information can be used in the model

ModelVOB <- randomForest(StdCE ~ .,
                       data=data_VO_bestuur,
                       ntree=1000,
                       localImp=TRUE)
ModelVOB

######## 3) Model Bestuur survey - selection####
#Based on the VIP determine the variables to be selected for each factor
#Normalize the variable importance to 100%
VariableImportances_boardVO <- data.frame(randomForest::importance(ModelVOB))
VariableImportances_boardVO$VIPnormalized <- VariableImportances_boardVO$X.IncMSE/sum(VariableImportances_boardVO$X.IncMSE)
VariableImportances_boardVO$VIPnormalizedFromR2 <- VariableImportances_boardVO$VIPnormalized*max(ModelVOB$rsq)
VariableImportances_boardVO <- tibble::rownames_to_column(VariableImportances_boardVO, "Variabele")
#Load categorized data
Categorizing <- read_excel("data/Categorisering_model_VO.xlsx", sheet = "Categorisering")
VariableImportances_boardVO <- merge(VariableImportances_boardVO,Categorizing,by="Variabele")
output <- VariableImportances_boardVO %>%
  group_by(Sub_Categorie) %>%
  mutate(rank = order(order(VIPnormalizedFromR2, decreasing=TRUE)))
#Keep most important variables per subcategory
output <- subset(output, rank>15)
output_to_drop <- as.vector(unique(c(output$Variabele)))

data_VO_bestuurselected <- data_VO_bestuur[,!names(data_VO_bestuur)%in%output_to_drop]

ModelVOBselected <- randomForest(StdCE ~ .,
                         data=data_VO_bestuurselected,
                         ntree=1000,
                         localImp=TRUE)
ModelVOB
