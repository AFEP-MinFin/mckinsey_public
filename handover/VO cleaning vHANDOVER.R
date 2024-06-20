#This script loads all the files needed for the analysis of VO and makes it one big dataframe with columns on BRIN6-level

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
library("tidyverse")
library("stringr")

####### Compute the VA per school, per year######

### 2015
###Determine average CE results manually for the 'raw' CE scores for our random forest model
StartScoresVO_all <- read.csv("Data/Kwaliteitsscores/VO/WISKUNDE_brin_vestiging_schoolsoortmetleerweg.csv",sep=",",stringsAsFactors = FALSE,dec = ".")
#Clean/adjust column names
StartScoresVO <- StartScoresVO_all[StartScoresVO_all$jaar==2015,]
StartScoresVO$X <- NULL
names(StartScoresVO)[names(StartScoresVO) == "brin"] <- "BRIN4"
StartScoresVO$BRIN6 <- paste(StartScoresVO$BRIN4,ifelse(nchar(StartScoresVO$vestiging)<2,
                      paste("0",StartScoresVO$vestiging,sep=""),StartScoresVO$vestiging),sep="")
StartScoresVO$vestiging <- NULL
names(StartScoresVO)[names(StartScoresVO) == "schoolsoort..met.leerweg."] <- "Schoolsoort"

StartScoresVOCito <- StartScoresVO[StartScoresVO$soort.eindtoets == "Cito Eindtoets  501-550 (tm 2014) ",]
StartScoresVOCito <- StartScoresVOCito[!is.na(StartScoresVOCito$gemiddelde.score.eindtoets),]

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(meanCITO = mean(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sdCITO = sd(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(mean = mean(gemiddelde.score.wiskunde.examen))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sd = sd(gemiddelde.score.wiskunde.examen))

#Determine average value add per school by multiplying with N per schoolsoort and then deviding over total N
AllScores <- StartScoresVOCito
AllScores$CITOnorm <- (AllScores$gemiddelde.score.eindtoets-AllScores$meanCITO)/AllScores$sdCITO
AllScores$norm <- (AllScores$gemiddelde.score.wiskunde.examen-AllScores$mean)/AllScores$sd
AllScores$ValueAdd <- AllScores$norm - AllScores$CITOnorm
AllScores$ValueAddN <- AllScores$ValueAdd*AllScores$n
AllScores$CITOnormN <- AllScores$CITOnorm*AllScores$n
AllScores$normN <- AllScores$norm*AllScores$n

VA2015 <- aggregate(list(n=AllScores$n,
                         valueaddn=AllScores$ValueAddN,
                         Std_CE_n=AllScores$normN,
                         Std_CITO_n=AllScores$CITOnormN),
                    by=list(BRIN6=AllScores$BRIN6),sum)

VA2015$ValueAddFin  <- VA2015$valueaddn / VA2015$n
VA2015$Std_CEFin <- VA2015$Std_CE_n / VA2015$n
VA2015$Std_CITOFin <- VA2015$Std_CITO_n / VA2015$n
VA2015$jaar <- 2015

VA2015 <- VA2015[, c("BRIN6", "ValueAddFin", "Std_CEFin", "Std_CITOFin", "jaar")]
write.csv(VA2015, file="Data/Kwaliteitsscores/VA2015.csv")

### 2016
###Determine average CE results manually for the 'raw' CE scores for our random forest model
StartScoresVO_all <- read.csv("Data/Kwaliteitsscores/VO/WISKUNDE_brin_vestiging_schoolsoortmetleerweg.csv",sep=",",stringsAsFactors = FALSE,dec = ".")
#Clean/adjust column names
StartScoresVO <- StartScoresVO_all[StartScoresVO_all$jaar==2016,]
StartScoresVO$X <- NULL
names(StartScoresVO)[names(StartScoresVO) == "brin"] <- "BRIN4"
StartScoresVO$BRIN6 <- paste(StartScoresVO$BRIN4,ifelse(nchar(StartScoresVO$vestiging)<2,
                                                        paste("0",StartScoresVO$vestiging,sep=""),StartScoresVO$vestiging),sep="")
StartScoresVO$vestiging <- NULL
names(StartScoresVO)[names(StartScoresVO) == "schoolsoort..met.leerweg."] <- "Schoolsoort"
StartScoresVOCito <- StartScoresVO[StartScoresVO$soort.eindtoets == "Cito Eindtoets  501-550 (tm 2014) ",]
StartScoresVOCito <- StartScoresVOCito[!is.na(StartScoresVOCito$gemiddelde.score.eindtoets),]
StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(meanCITO = mean(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sdCITO = sd(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(mean = mean(gemiddelde.score.wiskunde.examen))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sd = sd(gemiddelde.score.wiskunde.examen))

#Determine average value add per school by multiplying with N per schoolsoort and then deviding over total N
AllScores <- StartScoresVOCito
AllScores$CITOnorm <- (AllScores$gemiddelde.score.eindtoets-AllScores$meanCITO)/AllScores$sdCITO
AllScores$norm <- (AllScores$gemiddelde.score.wiskunde.examen-AllScores$mean)/AllScores$sd
AllScores$ValueAdd <- AllScores$norm - AllScores$CITOnorm

AllScores$ValueAddN <- AllScores$ValueAdd*AllScores$n
AllScores$CITOnormN <- AllScores$CITOnorm*AllScores$n
AllScores$normN <- AllScores$norm*AllScores$n

VA2016 <- aggregate(list(n=AllScores$n,
                         valueaddn=AllScores$ValueAddN,
                         Std_CE_n=AllScores$normN,
                         Std_CITO_n=AllScores$CITOnormN),
                    by=list(BRIN6=AllScores$BRIN6),sum)

VA2016$ValueAddFin  <- VA2016$valueaddn / VA2016$n
VA2016$Std_CEFin <- VA2016$Std_CE_n / VA2016$n
VA2016$Std_CITOFin <- VA2016$Std_CITO_n / VA2016$n
VA2016$jaar <- 2016

VA2016 <- VA2016[, c("BRIN6", "ValueAddFin", "Std_CEFin", "Std_CITOFin", "jaar")]
write.csv(VA2016, file="Data/Kwaliteitsscores/VA2016.csv")

### 2017
###Determine average CE results manually for the 'raw' CE scores for our random forest model
StartScoresVO_all <- read.csv("Data/Kwaliteitsscores/VO/WISKUNDE_brin_vestiging_schoolsoortmetleerweg.csv",sep=",",stringsAsFactors = FALSE,dec = ".")
#Clean/adjust column names
StartScoresVO <- StartScoresVO_all[StartScoresVO_all$jaar==2017,]
StartScoresVO$X <- NULL
names(StartScoresVO)[names(StartScoresVO) == "brin"] <- "BRIN4"
StartScoresVO$BRIN6 <- paste(StartScoresVO$BRIN4,ifelse(nchar(StartScoresVO$vestiging)<2,
                                                        paste("0",StartScoresVO$vestiging,sep=""),StartScoresVO$vestiging),sep="")
StartScoresVO$vestiging <- NULL
names(StartScoresVO)[names(StartScoresVO) == "schoolsoort..met.leerweg."] <- "Schoolsoort"
StartScoresVOCito <- StartScoresVO[StartScoresVO$soort.eindtoets == "Cito Eindtoets  501-550 (tm 2014) ",]
StartScoresVOCito <- StartScoresVOCito[!is.na(StartScoresVOCito$gemiddelde.score.eindtoets),]

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(meanCITO = mean(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sdCITO = sd(gemiddelde.score.eindtoets))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(mean = mean(gemiddelde.score.wiskunde.examen))

StartScoresVOCito <- StartScoresVOCito %>%
  group_by(Schoolsoort, wiskundesoort) %>%
  mutate(sd = sd(gemiddelde.score.wiskunde.examen))

#Determine average value add per school by multiplying with N per schoolsoort and then deviding over total N
AllScores <- StartScoresVOCito
AllScores$CITOnorm <- (AllScores$gemiddelde.score.eindtoets-AllScores$meanCITO)/AllScores$sdCITO
AllScores$norm <- (AllScores$gemiddelde.score.wiskunde.examen-AllScores$mean)/AllScores$sd
AllScores$ValueAdd <- AllScores$norm - AllScores$CITOnorm

AllScores$ValueAddN <- AllScores$ValueAdd*AllScores$n
AllScores$CITOnormN <- AllScores$CITOnorm*AllScores$n
AllScores$normN <- AllScores$norm*AllScores$n

VA2017 <- aggregate(list(n=AllScores$n,
                         valueaddn=AllScores$ValueAddN,
                         Std_CE_n=AllScores$normN,
                         Std_CITO_n=AllScores$CITOnormN),
                    by=list(BRIN6=AllScores$BRIN6),sum)

VA2017$ValueAddFin  <- VA2017$valueaddn / VA2017$n
VA2017$Std_CEFin <- VA2017$Std_CE_n / VA2017$n
VA2017$Std_CITOFin <- VA2017$Std_CITO_n / VA2017$n
VA2017$jaar <- 2017

VA2017 <- VA2017[, c("BRIN6", "ValueAddFin", "Std_CEFin", "Std_CITOFin", "jaar")]
write.csv(VA2017, file="Data/Kwaliteitsscores/VA2017.csv")


VA2015 <- read.csv(file="Data/Kwaliteitsscores/VA2015.csv")
VA2016 <- read.csv(file="Data/Kwaliteitsscores/VA2016.csv")
VA2017 <- read.csv(file="Data/Kwaliteitsscores/VA2017.csv")

VA_all <- rbind(VA2015,VA2016, VA2017)

VA_all$X <- NULL
# Count number of years per school
VA_all$count <- 1
# Construct 1 indicator
VA_all <- VA_all %>%
  group_by(BRIN6) %>%
  mutate(max = max(ValueAddFin))
VA_all <- VA_all %>%
  group_by(BRIN6) %>%
  mutate(min = min(ValueAddFin))
VA_all$range <- VA_all$max - VA_all$min
VA <- aggregate(list(years=VA_all$count,
                     VA=VA_all$ValueAddFin,
                     range=VA_all$range,
                     StdCE=VA_all$Std_CEFin,
                     StdCITO=VA_all$Std_CITOFin),
                by=list(BRIN6=VA_all$BRIN6),sum)
VA$VA <- VA$VA/VA$years
VA$StdCE <- VA$StdCE/VA$years
VA$StdCITO <- VA$StdCITO/VA$years
VA$range <- VA$range/VA$years
table(VA$years)
  # 1016 unique schools for 2015-16-17 data
VA <- subset(VA, years>2)
VA <- subset(VA, abs(VA)<3)
  # deleting outliers in VA
VA <- subset(VA, abs(range)<3)
  # deleting outliers in range (jumps in VA)
  #989 after leaving out outliers in terms of VA & range

#### CORRECTED SCORES FOR TOP-BOTTOM ANALYSIS ####
#Determine the VA based on Inspectie CE scores and corrections for op/afstroom, as used in top-bottom analysis

#Load VO CE scores with the corrections of the Inspectie van het Onderwijs for each of the schooltypen
inspectie_VMBOB <- read_excel("Data/inspectie.xlsx", sheet = "VMBO B",skip = 1)
inspectie_VMBOK <- read_excel("Data/inspectie.xlsx", sheet = "VMBO K",skip = 1)
inspectie_VMBOGT <- read_excel("Data/inspectie.xlsx", sheet = "VMBO GT",skip = 1)
inspectie_HAVO <- read_excel("Data/inspectie.xlsx", sheet = "HAVO",skip = 1)
inspectie_VWO <- read_excel("Data/inspectie.xlsx", sheet = "VWO",skip = 1)

#For each of the types, correct the CE score for the OBadvies (the opstroom & afstroom)
inspectie_VMBOB <- inspectie_VMBOB[,c(1,2,3,8)] #Load the CE scores, the N, and the OBadvies (percentile score based on op- and afstroom)
colnames(inspectie_VMBOB) <- c("BRIN6","CE","CE_aantal_VMBOB","OBadvies") #Give correct column names
modelVMBOB <- lm(inspectie_VMBOB$CE ~ inspectie_VMBOB$OBadvies, data=inspectie_VMBOB, na.action = na.exclude) #Determine the relation between CE scores and opstroom/afstroom
inspectie_VMBOB$CEcorrected_VMBOB <- percent_rank(((inspectie_VMBOB$CE-predict(modelVMBOB))))*100 #Determine the new percentiel score

inspectie_VMBOK <- inspectie_VMBOK[,c(1,2,3,8)]
colnames(inspectie_VMBOK) <- c("BRIN6","CE","CE_aantal_VMBOK","OBadvies")
modelVMBOK <- lm(inspectie_VMBOK$CE ~ inspectie_VMBOK$OBadvies, data=inspectie_VMBOK, na.action = na.exclude)
inspectie_VMBOK$CEcorrected_VMBOK <- percent_rank(((inspectie_VMBOK$CE-predict(modelVMBOK))))*100

inspectie_VMBOGT <- inspectie_VMBOGT[,c(1,2,3,8)]
colnames(inspectie_VMBOGT) <- c("BRIN6","CE","CE_aantal_VMBOGT","OBadvies")
modelVMBOGT <- lm(inspectie_VMBOGT$CE ~ inspectie_VMBOGT$OBadvies, data=inspectie_VMBOGT, na.action = na.exclude)
inspectie_VMBOGT$CEcorrected_VMBOGT <- percent_rank(((inspectie_VMBOGT$CE-predict(modelVMBOGT))))*100

inspectie_HAVO <- inspectie_HAVO[,c(1,2,3,8)]
colnames(inspectie_HAVO) <- c("BRIN6","CE","CE_aantal_HAVO","OBadvies")
modelHAVO <- lm(inspectie_HAVO$CE ~ inspectie_HAVO$OBadvies, data=inspectie_HAVO, na.action = na.exclude)
inspectie_HAVO$CEcorrected_HAVO <- percent_rank(((inspectie_HAVO$CE-predict(modelHAVO))))*100

inspectie_VWO <- inspectie_VWO[,c(1,2,3,8)]
colnames(inspectie_VWO) <- c("BRIN6","CE","CE_aantal_VWO","OBadvies")
modelVWO <- lm(inspectie_VWO$CE ~ inspectie_VWO$OBadvies, data=inspectie_VWO, na.action = na.exclude)
inspectie_VWO$CEcorrected_VWO <- percent_rank(((inspectie_VWO$CE-predict(modelVWO))))*100

#Determine per school the weighted average of the corrected CE scores
#Merge to one file
AllInspectie <- merge(inspectie_VMBOB[,c("BRIN6","CEcorrected_VMBOB","CE_aantal_VMBOB")],
                      merge(inspectie_VMBOK[,c("BRIN6","CEcorrected_VMBOK","CE_aantal_VMBOK")],
                            merge(inspectie_VMBOGT[,c("BRIN6","CEcorrected_VMBOGT","CE_aantal_VMBOGT")],
                                  merge(inspectie_HAVO[,c("BRIN6","CEcorrected_HAVO","CE_aantal_HAVO")],
                                        inspectie_VWO[,c("BRIN6","CEcorrected_VWO","CE_aantal_VWO")],all = TRUE),all=TRUE),all = TRUE),all = TRUE)
AllInspectie[is.na(AllInspectie)] <- 0 #Make all NAs zero so we can add and divide the totals
#Multiply the corrected CE scores by N for each school type and then divide by total N
AllInspectie$CEtotaal <-
  (AllInspectie$CEcorrected_VMBOB*AllInspectie$CE_aantal_VMBOB+
     AllInspectie$CEcorrected_VMBOK*AllInspectie$CE_aantal_VMBOK+
     AllInspectie$CEcorrected_VMBOGT*AllInspectie$CE_aantal_VMBOGT+
     AllInspectie$CEcorrected_HAVO*AllInspectie$CE_aantal_HAVO+
     AllInspectie$CEcorrected_VWO*AllInspectie$CE_aantal_VWO)/
  (AllInspectie$CE_aantal_VMBOB+AllInspectie$CE_aantal_VMBOK+AllInspectie$CE_aantal_VMBOGT+AllInspectie$CE_aantal_HAVO+AllInspectie$CE_aantal_VWO)
AllInspectie$CEtotaal <- ifelse(AllInspectie$CEtotaal == 0,NA,AllInspectie$CEtotaal)

########LOAD ALL DATA PER BRIN6######
# Personeel
#Load the characteristics of personeel: The % of FTE that is part of a functiegroep
PersoneelVO <- read_excel("Data/02-onderwijspersoneel-vo-in-fte-2011-2018.xlsx",sheet = "per owtype-bestuur-brin-functie")
PersoneelVO <- PersoneelVO[,c("FUNCTIEGROEP","BRIN NUMMER","FTE'S 2017","GEMIDDELDE LEEFTIJD 2017")]
#Clean the columns
PersoneelVO$BRIN4 <- PersoneelVO$`BRIN NUMMER`
PersoneelVO$FTE <- PersoneelVO$`FTE'S 2017`
PersoneelVO <- cast(PersoneelVO,BRIN4~FUNCTIEGROEP,value = "FTE",sum)
PersoneelVO$Directie <- ifelse(is.nan(PersoneelVO$Directie),0,PersoneelVO$Directie)
PersoneelVO$`Leraren in opleiding (LIO)` <- ifelse(is.nan(PersoneelVO$`Leraren in opleiding (LIO)`),0,PersoneelVO$`Leraren in opleiding (LIO)`)
PersoneelVO$Onbekend <- ifelse(is.nan(PersoneelVO$Onbekend),0,PersoneelVO$Onbekend)
PersoneelVO$`Onderwijsgevend personeel` <- ifelse(is.nan(PersoneelVO$`Onderwijsgevend personeel`),
                                                  0,PersoneelVO$`Onderwijsgevend personeel`)
PersoneelVO$`Onderwijsondersteunend personeel (OOP/OBP)` <- ifelse(is.nan(PersoneelVO$`Onderwijsondersteunend personeel (OOP/OBP)`),
                                                                   0,PersoneelVO$`Onderwijsondersteunend personeel (OOP/OBP)`)

#Determine % of total per functiegroep
PersoneelVO$Totaal <- PersoneelVO$Directie+PersoneelVO$`Leraren in opleiding (LIO)`+
                      PersoneelVO$Onbekend+PersoneelVO$`Onderwijsgevend personeel`+
                      PersoneelVO$`Onderwijsondersteunend personeel (OOP/OBP)`
PersoneelVO <- PersoneelVO[PersoneelVO$Totaal!=0,]
PersoneelVO$total <- PersoneelVO$Directie+PersoneelVO$`Onderwijsgevend personeel`+
                      PersoneelVO$`Onderwijsondersteunend personeel (OOP/OBP)`
for (i in (2:6)) {
  PersoneelVO[,i] <- PersoneelVO[,i]/PersoneelVO$total
}
PersoneelVO <- PersoneelVO[,c("BRIN4","Directie","Onderwijsgevend personeel","Onderwijsondersteunend personeel (OOP/OBP)")] #Could later also take a look at the leraren in opleiding (LIO)

#Load details van personeel for their ages and whether working fulltime
PersoneelVO2 <- read_excel("Data/02-onderwijspersoneel-vo-in-fte-2011-2018.xlsx",sheet = "per owtype-bestuur-brin")
PersoneelVO2 <- PersoneelVO2[,c("BRIN NUMMER","FTE'S 2018","FTE'S PERSONEN IN VASTE DIENST 2018","FTE'S PERSONEN IN TIJDELIJKE DIENST 2017","FTE'S VROUWEN 2017","FTE'S MANNEN 2017","FTE'S PERSONEN JONGER DAN 15 JAAR 2017","FTE'S PERSONEN 15 - 25 JAAR 2017","FTE'S PERSONEN 25 - 35 JAAR 2017","FTE'S PERSONEN 35 - 45 JAAR 2017","FTE'S PERSONEN 45 - 55 JAAR 2017","FTE'S PERSONEN 55 - 65 JAAR 2017","FTE'S PERSONEN 65 JAAR EN OUDER 2017","FTE'S PERSONEN 0 - 0.5 FTE'S 2017","FTE'S PERSONEN 0.5 - 0.8 FTE'S 2017","FTE'S PERSONEN MEER DAN 0.8 FTE'S 2017","GEMIDDELDE FTE'S 2017","GEMIDDELDE LEEFTIJD 2017")]
names(PersoneelVO2)[names(PersoneelVO2) == "BRIN NUMMER"] <- "BRIN4"
for (i in (3:16)) {
  PersoneelVO2[,i] <- PersoneelVO2[,i]/PersoneelVO2$`FTE'S 2018`
}

### Load Verzuimgetallen
VerzuimKengetallenVO <- read_excel("Data/verzuimkengetallen-2015-2017.xlsx",sheet = "bestuur brin")
VerzuimKengetallenVO <- VerzuimKengetallenVO[,c("sector","bestuur","brin","VP17","MF17","GZD17")]
VerzuimKengetallenVO <- VerzuimKengetallenVO[VerzuimKengetallenVO$sector=="vo",]
#Clean column names
names(VerzuimKengetallenVO)[names(VerzuimKengetallenVO) == "brin"] <- "BRIN4"
names(VerzuimKengetallenVO)[names(VerzuimKengetallenVO) == "VP17"] <- "VerzuimPercentage"
names(VerzuimKengetallenVO)[names(VerzuimKengetallenVO) == "MF17"] <- "MeldingsFrequentie"
names(VerzuimKengetallenVO)[names(VerzuimKengetallenVO) == "GZD17"] <- "GemiddeldeZiekteverzuimDuur"
VerzuimKengetallenVO <- VerzuimKengetallenVO[,c("BRIN4","VerzuimPercentage","MeldingsFrequentie",
                                                "GemiddeldeZiekteverzuimDuur")]

### Load number of leerlingen
LeerlingenVO <- read_excel("Data/01-leerlingen-vo-per-vestiging-naar-onderwijstype-2017.xlsx", sheet = "Sheet1")
LeerlingenVO <- LeerlingenVO[,-c(3:5)]
LeerlingenVO <- LeerlingenVO[,-c(4:7)]
LeerlingenVO$BRIN6 <- paste(LeerlingenVO$`BRIN NUMMER`,LeerlingenVO$VESTIGINGSNUMMER,sep="")
LeerlingenVO$`BRIN NUMMER`<- NULL
LeerlingenVO$VESTIGINGSNUMMER<- NULL
names(LeerlingenVO)[names(LeerlingenVO) == "ONDERWIJSTYPE VO EN LEER- OF VERBLIJFSJAAR"] <- "naam_jaar"
LeerlingenVO$total <- rowSums(LeerlingenVO[2:13])
LeerlingenVO <- LeerlingenVO[,c("naam_jaar", "BRIN6", "total")]
LeerlingenVO <- cast(LeerlingenVO,BRIN6~naam_jaar,value = "total",sum)
LeerlingenVO$VMBO <- LeerlingenVO$`VMBO-MBO2 lj 3-6`+
                     LeerlingenVO$`VMBO BL lj 3-4`+
                     LeerlingenVO$`VMBO GL lj 3-4`+
                     LeerlingenVO$`VMBO KL lj 3-4`+
                     LeerlingenVO$`VMBO TL lj 3-4`+
                     LeerlingenVO$`VMBO uitbest. aan VAVO`
LeerlingenVO$HAVO <- LeerlingenVO$`HAVO lj 4-5`+
                     LeerlingenVO$`HAVO uitbest. aan VAVO`

#Determine % for each schooltype
LeerlingenVO$TOTAAL <- rowSums(LeerlingenVO[2:17])
for (i in (2:19)) {
  LeerlingenVO[,i] <- LeerlingenVO[,i]/LeerlingenVO$TOTAAL
}

LeerlingenVO <- LeerlingenVO[,c('BRIN6',"VMBO","HAVO", "TOTAAL")]

### Load impulsgebieden, will be merged later
Impulsgebieden <- read.csv("Data/Impulsgebieden.csv")

### Load CBS gemeentescores
GemeenteCodes <- read_excel("Data/Gemeentescores/Gemeenten alfabetisch 2019.xls")
GemeenteCodes <- GemeenteCodes[,c("GemeentecodeGM","Gemeentenaam")]
names(GemeenteCodes)[names(GemeenteCodes) == "GemeentecodeGM"] <- "GBD"
GemeenteScores <- read_excel("Data/Gemeentescores/dimensiescore_gemeente (stand).xlsx")
GemeenteScores <- GemeenteScores[GemeenteScores$JAAR == 2016,]
GemeenteOntwikkeling <- read_excel("Data/Gemeentescores/dimensiescore_gemeente (ontwikkeling).xlsx")
GemeenteOntwikkeling <- GemeenteOntwikkeling[GemeenteOntwikkeling$BEGIN==2012&GemeenteOntwikkeling$EINDE==2016,]
#Merge together for the scores per gemeente
Gemeente <- GemeenteCodes
Gemeente <- merge(Gemeente,GemeenteScores)
Gemeente <- merge(Gemeente,GemeenteOntwikkeling)
Gemeente <- subset(Gemeente, select = -c(GBD,JAAR,BEGIN,EINDE))
Gemeente$Gemeentenaam <- toupper(Gemeente$Gemeentenaam)
names(Gemeente)[names(Gemeente)=="Gemeentenaam"]<-"GEMEENTENAAM"

### Krimp and groei
#Determine krimp/groei for each school region and clean the dataset
KrimpregioVO <- read_excel("Data/12.-leerlingenprognose-groei-krimp-2011-2036.xlsx",skip = 19)
KrimpregioVO <- KrimpregioVO[,c("GEMEENTENAAM INSTELLING","Procentuele verandering 2016-2021...16")]
names(KrimpregioVO)[names(KrimpregioVO) == "Procentuele verandering 2016-2021...16"] <- "GroeiLeerlingenGemeente20162021"
KrimpregioVO$krimp <- ifelse(KrimpregioVO$GroeiLeerlingenGemeente20162021< -0.075,TRUE,FALSE)
names(KrimpregioVO)[names(KrimpregioVO)=="GEMEENTENAAM INSTELLING"]<-"GEMEENTENAAM"
KrimpregioVO <- KrimpregioVO[,c("GEMEENTENAAM", "GroeiLeerlingenGemeente20162021", 'krimp')]
KrimpregioVO <- KrimpregioVO[!duplicated(KrimpregioVO$GEMEENTENAAM),]

### Load doublures (zittenblijven)
DoubluresVO <- read.csv("Data/05-zittenblijvers-niveau-vestiging-2017-2018-tov-2018-2019.csv", sep = ";")
DoubluresVO <- DoubluresVO[,c("BRINNUMMER","VESTIGINGSNUMMER","AANTAL_LEERLINGEN","AANTAL_ZITTENBLIJVERS")]
names(DoubluresVO)[names(DoubluresVO) == "BRINNUMMER"] <- "BRIN4"
DoubluresVO$BRIN6 <- paste(DoubluresVO$BRIN4,0,DoubluresVO$VESTIGINGSNUMMER,sep="")
DoubluresVO$VESTIGINGSNUMMER<-NULL
DoubluresVO$ZITPERC18 <- DoubluresVO$AANTAL_ZITTENBLIJVERS/DoubluresVO$AANTAL_LEERLINGEN
DoubluresVO <- DoubluresVO[, c("BRIN6", "ZITPERC18")]

### Load vroegtijdig schoolverlaters
VSV <- read_excel("Data/vsv.xlsx")
VSV$BRIN6 <- paste(VSV$brin_nummer,VSV$vestigings_nummer,sep="")
VSV$VSV <-VSV$vsv_totaal/VSV$aantal_leerlingen
VSV <- VSV[,c("BRIN6","VSV")]

# Load financials on board level
#Financiele kengetallen
FinancieleKengetallenVO <- read_excel("Data/16-financiele-kengetallen-per-bestuur-2013-2017.xlsx",sheet = " Kengetallen")
FinancieleKengetallenVO <- FinancieleKengetallenVO[,c("Bevoegd Gezag","Beleidsterrein","Jaar","Huisvestingsratio","Liquiditeit (current ratio)","Liquiditeit (quick ratio)","Personeelslasten / rijksbijdragen","Personeelslasten / totaal baten plus financi?le baten","Personeelslasten / totaal lasten plus financi?le lasten","Rentabiliteit","Solvabiliteit I","Solvabiliteit II","Weerstandsvermogen","Kapitalisatiefactor","Weerstandsvermogen exclusief materi?le vaste activa","Algemene reserve / totaal baten plus financi?le baten","Voorzieningen / totaal baten plus financi?le baten","Rijksbijdragen / totaal baten plus financi?le baten","Overige overheidsbijdragen en -subsidies / totaal baten plus financi?le baten","Investeringen huisvesting / totaal baten plus financi?le baten","Investeringen inventaris en apparatuur / totaal baten plus financi?le baten","Netto werkkapitaal","Beleggingen ten opzichte van het eigen vermogen","Contractactiviteiten / Rijksbijdragen", "Contractactiviteiten / totaal baten plus financi?le baten")]
FinancieleKengetallenVO[is.na(FinancieleKengetallenVO)] <- 9999
      # all missing values are labelled as 9999 so this information can still be used in the model
names(FinancieleKengetallenVO)[names(FinancieleKengetallenVO) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"

#Overheidsbijdragen
OverheidsbijdragenVO <- read_excel("Data/12-overheidsbijdragen-2013-2017.xlsx")
OverheidsbijdragenVO <- OverheidsbijdragenVO[OverheidsbijdragenVO$Beleidsterrein == "VO",]
OverheidsbijdragenVO <- OverheidsbijdragenVO[OverheidsbijdragenVO$Jaar == 2017,]
names(OverheidsbijdragenVO)[names(OverheidsbijdragenVO) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
OverheidsbijdragenVO[is.na(OverheidsbijdragenVO)] <- 0
      # all missing financial values are labelled as 0 so this information can still be used in the model

#Overige baten
OverigeBatenVO <- read_excel("Data/13-overige-baten-2013-2017.xlsx")
OverigeBatenVO <- OverigeBatenVO[OverigeBatenVO$Beleidsterrein == "VO",]
names(OverigeBatenVO)[names(OverigeBatenVO) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
OverigeBatenVO <- OverigeBatenVO[OverigeBatenVO$Jaar == 2017,]
OverigeBatenVO[is.na(OverigeBatenVO)] <- 0
# all missing financial values are labelled as 0 so this information can still be used in the model

#Lasten
Lasten <- read_excel("Data/14-lasten-2013-2017.xlsx")
LastenVO <- Lasten[Lasten$Beleidsterrein == "VO",]
names(LastenVO)[names(LastenVO) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
LastenVO <- LastenVO[LastenVO$Jaar == 2017,]
LastenVO[is.na(LastenVO)] <- 0
# all missing financial values are labelled as 0 so this information can still be used in the model

#Merge total financien on board level
FinancienBestuurVO <- merge(FinancieleKengetallenVO,OverheidsbijdragenVO)
FinancienBestuurVO <- merge(FinancienBestuurVO,OverigeBatenVO)
FinancienBestuurVO <- merge(FinancienBestuurVO,LastenVO)
FinancienBestuurVO$Beleidsterrein<-NULL
FinancienBestuurVO$Jaar<-NULL
FinancienBestuurVO$Groepering<-NULL
FinancienBestuurVO$`Naam Kort`<-NULL


# Drop alle rijksbijdragen & baten (~school composition)
FinancienBestuurVO$`Personeelslasten / rijksbijdragen` <- NULL
FinancienBestuurVO$`Personeelslasten / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Algemene reserve / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Voorzieningen / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Rijksbijdragen / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Overige overheidsbijdragen en -subsidies / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Investeringen huisvesting / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Investeringen inventaris en apparatuur / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Contractactiviteiten / Rijksbijdragen` <- NULL
FinancienBestuurVO$`Contractactiviteiten / totaal baten plus financi?le baten` <- NULL
FinancienBestuurVO$`Rijksbijdragen OCW` <- NULL
FinancienBestuurVO$`Rijksbijdragen EZ` <- NULL
FinancienBestuurVO$`Rijksbijdragen OCW/EZ` <- NULL
FinancienBestuurVO$`Rijksbijdrage academische ziekenhuizen` <- NULL
FinancienBestuurVO$`Doorbetalingen rijksbijdrage SWV` <- NULL
FinancienBestuurVO$`Inkomensoverdracht van rijksbijdragen` <- NULL
FinancienBestuurVO$`Ontvangen doorbetalingen Rijksbijdrage SWV` <- NULL
FinancienBestuurVO$`Overige rijksbijdragen` <- NULL
FinancienBestuurVO$Rijksbijdragen <- NULL

##### Generate master file with all characteristics and survey data ####
setwd(paste("C:/Users/",Sys.info()[7],
            "/Box Sync/OCW - Onderzoek doelmatigheid & toereikendheid/4. Working folder/3. Wouter/Doelmatigheidsanalyse", sep = "",""))

survey_BG <- read.csv(file = paste("C:/Users/",Sys.info()[7],
                                   "/Box Sync/OCW - Onderzoek doelmatigheid & toereikendheid/4. Working folder/4. Fritz/99. School dashboards/Spilios Folder/01.Data/survey_BG.csv",sep = "",""))
survey_BG <- survey_BG[,-c(1)]
survey_BRIN6 <- read.csv(file = paste("C:/Users/",Sys.info()[7],
                                      "/Box Sync/OCW - Onderzoek doelmatigheid & toereikendheid/4. Working folder/4. Fritz/99. School dashboards/Spilios Folder/01.Data/survey_BRIN6.csv",sep = "",""))
survey_BRIN6 <- survey_BRIN6[,-c(1)]

VO_all <- read.csv("Data/02-alle-vestigingen-vo.csv", sep = ";")
VO_all <- VO_all[, c('BEVOEGD.GEZAG.NUMMER','BRIN.NUMMER',"VESTIGINGSNUMMER", "GEMEENTENAAM","POSTCODE")]
colnames(VO_all) <- c('BEVOEGD.GEZAG.NUMMER','BRIN4',"BRIN6","GEMEENTENAAM","POSTCODE")
VO_all$GEMEENTENAAM <- as.character(VO_all$GEMEENTENAAM)
VO_all$BRIN4 <- as.character(VO_all$BRIN4)
VO_all$BRIN6 <- as.character(VO_all$BRIN6)

#Determine number of schools per BRIN4
VO_all <- VO_all %>%
  group_by(BRIN4) %>%
  mutate(BRIN6_per_BRIN4 = n_distinct(BRIN6))
table(VO_all$BRIN6_per_BRIN4)

#Determine number of schools per bevoegd gezag
VO_all <- VO_all %>%
  group_by(BEVOEGD.GEZAG.NUMMER) %>%
  mutate(BRIN6_per_BG = n_distinct(BRIN6))
table(VO_all$BRIN6_per_BG)
hist(VO_all$BRIN6_per_BG)

### Combine datasets
data_VO <- VO_all
data_VO <- merge(data_VO, VA, by='BRIN6', all.x = TRUE)
data_VO_merged <- merge(data_VO, AllInspectie,
                        by="BRIN6", all.x = TRUE, all.y = FALSE)
data_VO_merged$VA <- data_VO_merged$CEtotaal
data_VO <- data_VO_merged
data_VO <- merge(data_VO, PersoneelVO, by='BRIN4', all.x = TRUE)
data_VO <- merge(data_VO, PersoneelVO2, by='BRIN4', all.x = TRUE)

data_VO <- merge(data_VO, LeerlingenVO, by="BRIN6", all.x = TRUE)

data_VO <- merge(data_VO, VerzuimKengetallenVO, by='BRIN4', all.x = TRUE)

data_VO <- merge(data_VO, Gemeente, by='GEMEENTENAAM', all.x = TRUE)
data_VO$IMPULSGEBIED <- ifelse(substr(data_VO$POSTCODE,1,4) %in% unlist(Impulsgebieden),1,0)

data_VO <- merge(data_VO, KrimpregioVO, by='GEMEENTENAAM', all.x = TRUE)

data_VO <- merge(data_VO, VSV, by='BRIN6', all.x = TRUE)

data_VO <- merge(data_VO, DoubluresVO, by='BRIN6', all.x = TRUE)

data_VO$BEVOEGD.GEZAG.NUMMER <- as.character(data_VO$BEVOEGD.GEZAG.NUMMER)

data_VO <- merge(data_VO, FinancienBestuurVO, by='BEVOEGD.GEZAG.NUMMER', all.x = TRUE)

# Students per school
LLN_per_BG_VO <- data_VO %>%
  group_by(BEVOEGD.GEZAG.NUMMER) %>%
  summarise(LLN_per_BG = sum(TOTAAL, na.rm = TRUE))
table(is.na(LLN_per_BG_VO$LLN_per_BG))
data_VO <- merge(data_VO, LLN_per_BG_VO, by='BEVOEGD.GEZAG.NUMMER', all.x = TRUE)

# rescale funding per school by number of students
for (i in (grep("^Overige subsidies OCW$", colnames(data_VO)):grep("^Overige baten$", colnames(data_VO)))) {
  data_VO[,i] <- data_VO[,i]/data_VO$LLN_per_BG
}

table(length(unique(data_VO$BRIN6)))
      # 1610 unique schools remain in the master dataset

### Adding Survey data
data_VO <- merge(data_VO, survey_BG, by="BEVOEGD.GEZAG.NUMMER", all.x = TRUE)
data_VO <- merge(data_VO, survey_BRIN6, by="BRIN6", all.x = TRUE)
summary(data_VO$director_exists)

names(data_VO)<-str_replace_all(names(data_VO), c(" " = "." , "," = "" ))
names(data_VO)<-str_replace_all(names(data_VO), c("'" = "_" , "," = "" ))
names(data_VO)<-str_replace_all(names(data_VO), c("/" = "_" , "," = "" ))
names(data_VO)<-str_replace_all(names(data_VO), c("[(]" = "_" , "," = "" ))
names(data_VO)<-str_replace_all(names(data_VO), c("[)]" = "_" , "," = "" ))
names(data_VO)<-str_replace_all(names(data_VO), c("-" = "_" , "," = "" ))

##### Save dataset, ready for analysis ####
write.csv(data_VO, "data_VO_all.csv")
