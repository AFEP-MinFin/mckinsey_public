#This script loads all the files needed for the analysis of BAO and makes it one big data/data_poframe with columns on BRIN6-level

####PREPARE R AND LOAD PACKAGES####
#Set the working directory to own folder
## verwijderd privacy

#Load required packages, if this gives errors you might have to first install these
library("readxl")
library("reshape")
library("randomForest")
library("pdp")
library("vip")
library("Hmisc")
library("dplyr")
library("Benchmarking")
library("swfscMisc")
library("tidyverse")
library("stringr")

################################################LOAD INDIVIDUAL SCHOOLS##########################################
#Load all BRIN-level schools and clean up column names

#PO
BRIN6RowsPO <- read.csv("data/data_po/03-alle-vestigingen-bo-adres.csv",sep = ";",stringsAsFactors = FALSE)
BRIN6RowsPO <- BRIN6RowsPO[,c("BEVOEGD.GEZAG.NUMMER","BRIN.NUMMER","VESTIGINGSNUMMER","VESTIGINGSNAAM","PLAATSNAAM","GEMEENTENAAM","DENOMINATIE")]
BRIN6RowsPO$BEVOEGD.GEZAG.NUMMER <- as.character(BRIN6RowsPO$BEVOEGD.GEZAG.NUMMER)
names(BRIN6RowsPO)[names(BRIN6RowsPO) == "VESTIGINGSNUMMER"] <- "BRIN6"
names(BRIN6RowsPO)[names(BRIN6RowsPO) == "BRIN.NUMMER"] <- "BRIN4"
#Determine number of schools per BRIN4
BRIN6RowsPO <- BRIN6RowsPO %>%
  group_by(BRIN4) %>%
  mutate(BRIN6_per_BRIN4 = n_distinct(BRIN6))
table(BRIN6RowsPO$BRIN6_per_BRIN4)

#Determine number of schools per bevoegd gezag
BRIN6RowsPO <- BRIN6RowsPO %>%
  group_by(BEVOEGD.GEZAG.NUMMER) %>%
  mutate(BRIN6_per_BG = n_distinct(BRIN6))
table(BRIN6RowsPO$BRIN6_per_BG)
hist(BRIN6RowsPO$BRIN6_per_BG)

#####################################DETERMINE VALUE ADDED PER SCHOOL#################################
#Load 2017/2018 scores eindtoets
EindScores18 <- read.csv("data/data_po/05-gemiddelde-eindscores-bo-sbo-2017-2018.csv",sep=";",stringsAsFactors = FALSE,dec = ",")
EindScores18 <- EindScores18[,c("BRIN_NUMMER","VESTIGINGSNUMMER","LEERJAAR_8","ONTHEFFING_REDEN_ND","CET_AANTAL","CET_GEM","IEP_AANTAL","IEP_GEM","ROUTE8_AANTAL","ROUTE8_GEM","DIA_AANTAL","DIA_GEM","AMN_AANTAL","AMN_GEM")]
EindScores18$jaar <- 2018

#Load 2016/2017 scores eindtoets
EindScores17 <- read.csv("data/data_po/05.-gemiddelde-eindtoetsscores-2016-2017.csv",sep=";",stringsAsFactors = FALSE,dec = ",")
EindScores17 <- EindScores17[,c("BRIN_NUMMER","VESTIGINGSNUMMER","LEERJAAR_8","ONTHEFFING_REDEN_ND","CET_AANTAL","CET_GEM","IEP_AANTAL","IEP_GEM","ROUTE8_AANTAL","ROUTE8_GEM","DIA_AANTAL","DIA_GEM","AMN_AANTAL","AMN_GEM")]
EindScores17$jaar <- 2017

#Load 2015/2016 scores eindtoets
EindScores16 <- read.csv("data/data_po/05.-gemiddelde-eindscores-bo-sbo-2015-2016.csv",sep=";",stringsAsFactors = FALSE,dec = ",")
EindScores16 <- EindScores16[,c("BRIN_NUMMER","VESTIGINGSNUMMER","LEERJAAR_8","ONTHEFFING_REDEN_ND","CET_AANTAL","CET_GEM","IEP_AANTAL","IEP_GEM","ROUTE8_AANTAL","ROUTE8_GEM")]
EindScores16$jaar <- 2016
EindScores16$DIA_AANTAL <- NA
EindScores16$DIA_GEM <- NA
EindScores16$AMN_AANTAL <- NA
EindScores16$AMN_GEM <- NA

EindScores <- rbind(EindScores18, EindScores17, EindScores16)
#Clean column names
names(EindScores)[names(EindScores) == "BRIN_NUMMER"] <- "BRIN4"
EindScores$BRIN6 <- paste(EindScores$BRIN4,0,EindScores$VESTIGINGSNUMMER,sep="")
EindScores$VESTIGINGSNUMMER <- NULL

#Clean eindscores
EindScores$ScoreAvailable <- !is.na(EindScores$CET_GEM)|!is.na(EindScores$IEP_GEM)|!is.na(EindScores$ROUTE8_GEM)|!is.na(EindScores$DIA_GEM)|!is.na(EindScores$AMN_GEM)

#Load and clean gemiddelde verwachte score for 20172018 from the CBS calculations
VerwachteSchoolscores15 <- read_excel("data/data_po/181109 Gemiddelde verwachte schoolscores-def.xlsx",sheet = "Tabel 1",skip = 3)
VerwachteSchoolscores15 <- VerwachteSchoolscores15[-c(1,6301:6304),] #Remove NA and nonsense text rows that were parsed on the end
VerwachteSchoolscores15$jaar <- 2016

VerwachteSchoolscores16 <- read_excel("data/data_po/181109 Gemiddelde verwachte schoolscores-def.xlsx",sheet = "Tabel 2",skip = 3)
VerwachteSchoolscores16 <- VerwachteSchoolscores16[-c(1,6315:6318),] #Remove NA and nonsense text rows that were parsed on the end
VerwachteSchoolscores16$jaar <- 2017

VerwachteSchoolscores17 <- read_excel("data/data_po/181109 Gemiddelde verwachte schoolscores-def.xlsx",sheet = "Tabel 3",skip = 3)
VerwachteSchoolscores17 <- VerwachteSchoolscores17[-c(1,6325:6328),] #Remove NA and nonsense text rows that were parsed on the end
VerwachteSchoolscores17$jaar <- 2018

# Bind all verwachtescores to one data/data_poframe and clean names
VerwachteSchoolscores <- rbind(VerwachteSchoolscores15, VerwachteSchoolscores16, VerwachteSchoolscores17)
names(VerwachteSchoolscores)[names(VerwachteSchoolscores)=="Gemiddelde score"]<- "Schoolweging"
names(VerwachteSchoolscores)[names(VerwachteSchoolscores) == "BRIN"] <- "BRIN4"
VerwachteSchoolscores$BRIN6 <- paste(VerwachteSchoolscores$BRIN4,substr(VerwachteSchoolscores$Vestiging,1,2),sep="")
VerwachteSchoolscores$BRIN4 <- NULL

#Merge the verwachte scores and the actual eindscores
scores_PO <- base::merge(EindScores,VerwachteSchoolscores,by=c("BRIN6", "jaar"), all=TRUE)
scores_PO$VerwachtenRealisatie <- ifelse(scores_PO$ScoreAvailable,TRUE,FALSE)
scores_PO$VerwachtenRealisatie[is.na(scores_PO$VerwachtenRealisatie)] <- FALSE

#Only calculate value-added for schools of which we know both the expected and realised scores
#missings schools are SO schools, excluded from this analysis, or schools of schoolweging has not been calculated by CBS
scores_PO <- scores_PO[scores_PO$VerwachtenRealisatie,]
scores_PO <- scores_PO[!is.na(scores_PO$Schoolweging),]

#Test if correlation indeed between Cito and the schoolweging
  summary(lm(scores_PO$CET_GEM ~ scores_PO$Schoolweging))

#Determine the score based on standard deviation for each eindtoets PO
#Only do this for the 3 largest test: Citpo, IEP and Route8

  scores_PO <- scores_PO %>%
    group_by(jaar) %>% #Normalize within year
    mutate(meanCET = mean(CET_GEM, na.rm = TRUE)) #Determine mean
  scores_PO <- scores_PO %>%
    group_by(jaar) %>%
    mutate(sdCET = sd(CET_GEM, na.rm = TRUE))  #Determine standard deviation

  scores_PO <- scores_PO %>%
    group_by(jaar) %>%
    mutate(meanIEP = mean(IEP_GEM, na.rm = TRUE))
  scores_PO <- scores_PO %>%
    group_by(jaar) %>%
    mutate(sdIEP = sd(IEP_GEM, na.rm = TRUE))

  scores_PO <- scores_PO %>%
    group_by(jaar) %>%
    mutate(meanROUTE8 = mean(ROUTE8_GEM, na.rm = TRUE))
  scores_PO <- scores_PO %>%
    group_by(jaar) %>%
    mutate(sdROUTE8 = sd(ROUTE8_GEM, na.rm = TRUE))

#Determine number of standard deviations from the mean
scores_PO$CETDiffMean <- ifelse(!is.na(scores_PO$CET_GEM),scores_PO$CET_GEM - scores_PO$meanCET,NA)
scores_PO$CETstddiff <- ifelse(!is.na(scores_PO$CET_GEM),scores_PO$CETDiffMean/scores_PO$sdCET,NA)
scores_PO$meanCET <- NULL
scores_PO$sdCET <- NULL

scores_PO$IEPDiffMean <- ifelse(!is.na(scores_PO$IEP_GEM),scores_PO$IEP_GEM - scores_PO$meanIEP,NA)
scores_PO$IEPstddiff <- ifelse(!is.na(scores_PO$IEP_GEM),scores_PO$IEPDiffMean/scores_PO$sdIEP,NA)
scores_PO$meanIEP <- NULL
scores_PO$sdIEP <- NULL

scores_PO$ROUTE8DiffMean <- ifelse(!is.na(scores_PO$ROUTE8_GEM),scores_PO$ROUTE8_GEM - scores_PO$meanROUTE8,NA)
scores_PO$ROUTE8stddiff <- ifelse(!is.na(scores_PO$ROUTE8_GEM),scores_PO$ROUTE8DiffMean/scores_PO$sdROUTE8,NA)
scores_PO$meanROUTE8 <- NULL
scores_PO$sdROUTE8 <- NULL

#For some schools, more than one endtoets is made. Compare the test with highest N
scores_PO$MultipleScores <- ifelse((as.integer(!is.na(scores_PO$CETstddiff))+as.integer(!is.na(scores_PO$IEPstddiff))+as.integer(!is.na(scores_PO$ROUTE8stddiff)))>1,TRUE,FALSE)
table(scores_PO$MultipleScores)
scores_PO$StdScore <- ifelse(!is.na(scores_PO$CETstddiff),scores_PO$CETstddiff,ifelse(!is.na(scores_PO$IEPstddiff),scores_PO$IEPstddiff,(ifelse(!is.na(scores_PO$ROUTE8stddiff),scores_PO$ROUTE8stddiff,NA))))

#Normalize schoolweging also to standard deviation, so these can be compared to standard deviations in realized eindtoets scores
scores_PO <- scores_PO[!is.na(scores_PO$StdScore) & scores_PO$StdScore != "ERROR",]
scores_PO <- scores_PO %>%
  group_by(jaar) %>%
  mutate(meanCBS = mean(Schoolweging, na.rm = TRUE)) #Determine mean
scores_PO <- scores_PO %>%
  group_by(jaar) %>%
  mutate(sdCBS = sd(Schoolweging, na.rm = TRUE)) #Determine standard deviation

#Determine standard deviations from the mean
scores_PO$StdSchoolweging <- -1*((scores_PO$Schoolweging-scores_PO$meanCBS)/scores_PO$sdCBS)
scores_PO$meanCBS <- NULL
scores_PO$sdCBS <- NULL

#Calculate value-added per year based on the voorspelde score and actual score
scores_PO_VA <- scores_PO[, c('BRIN6', 'StdScore', 'StdSchoolweging', 'jaar')]
scores_PO_VA$ValueAdd <- scores_PO_VA$StdScore-scores_PO_VA$StdSchoolweging

#Determine range per school based on highest and lowest VA in the years
scores_PO_VA$count <- 1
scores_PO_VA <- scores_PO_VA %>%
  group_by(BRIN6) %>%
  mutate(max = max(ValueAdd))
scores_PO_VA <- scores_PO_VA %>%
  group_by(BRIN6) %>%
  mutate(min = min(ValueAdd))
scores_PO_VA$range <- scores_PO_VA$max - scores_PO_VA$min

# Aggregate multiple years into one score to have a more robust value-added
scores_PO_VA <- aggregate(list(years=scores_PO_VA$count,
                               VA=scores_PO_VA$ValueAdd,
                               range=scores_PO_VA$range,
                               StdScore=scores_PO_VA$StdScore,
                               StdSchoolweging=scores_PO_VA$StdSchoolweging),
                          by=list(BRIN6=scores_PO_VA$BRIN6),sum)
scores_PO_VA$VA <- scores_PO_VA$VA/scores_PO_VA$years
scores_PO_VA$range <- scores_PO_VA$range/scores_PO_VA$years
scores_PO_VA$StdScore <- scores_PO_VA$StdScore/scores_PO_VA$years
scores_PO_VA$StdSchoolweging <- scores_PO_VA$StdSchoolweging/scores_PO_VA$years

# 5827 unique schools for 2015-16-17 data/data_po
# Remove outliers (identical selection as in VO)
scores_PO_VA <- subset(scores_PO_VA, years>2)
scores_PO_VA <- subset(scores_PO_VA, abs(VA)<3)
scores_PO_VA <- subset(scores_PO_VA, abs(range)<3)

######################################### MERGE EXPLANATORY VARIABLES #######################################
# PERSONEEL
#Load and clean FTE personeel
Personeel <- read_excel("data/data_po/02-onderwijspersoneel-po-in-fte.xlsx",sheet = "per owtype-bestuur-brin-functie")
Personeel <- Personeel[,c("FUNCTIEGROEP","BRIN NUMMER","FTE'S 2018","GEMIDDELDE LEEFTIJD 2018")]
Personeel$BRIN4 <- Personeel$`BRIN NUMMER`
Personeel$FTE <- Personeel$`FTE'S 2018`
Personeel <- cast(Personeel,BRIN4~FUNCTIEGROEP,value = "FTE",sum)

#Determine FTE as % of total per BRIN4
Personeel$Totaal <- Personeel$Directie+Personeel$`Leraren in opleiding (LIO)`+Personeel$Onbekend+Personeel$`Onderwijsgevend personeel`+Personeel$`Onderwijsondersteunend personeel (OOP/OBP)`
Personeel <- Personeel[Personeel$Totaal!=0,]
Personeel$total <- Personeel$Directie+Personeel$`Onderwijsgevend personeel`+Personeel$`Onderwijsondersteunend personeel (OOP/OBP)`
for (i in (2:6)) {
  Personeel[,i] <- Personeel[,i]/Personeel$total
}
Personeel <- Personeel[,c("BRIN4","Directie","Onderwijsgevend personeel","Onderwijsondersteunend personeel (OOP/OBP)")] #Could later also take a look at the leraren in opleiding (LIO)

#Load details van personeel for their ages and whether working fulltime, as % of total FTE per BRIN4
Personeel2 <- read_excel("data/data_po/02-onderwijspersoneel-po-in-fte.xlsx",sheet = "per owtype-bestuur-brin")
Personeel2 <- Personeel2[,c("ONDERWIJSTYPE","BRIN NUMMER","FTE'S 2018","FTE'S PERSONEN IN VASTE DIENST 2018","FTE'S PERSONEN IN TIJDELIJKE DIENST 2017","FTE'S VROUWEN 2017","FTE'S MANNEN 2017","FTE'S PERSONEN JONGER DAN 15 JAAR 2017","FTE'S PERSONEN 15 - 25 JAAR 2017","FTE'S PERSONEN 25 - 35 JAAR 2017","FTE'S PERSONEN 35 - 45 JAAR 2017","FTE'S PERSONEN 45 - 55 JAAR 2017","FTE'S PERSONEN 55 - 65 JAAR 2017","FTE'S PERSONEN 65 JAAR EN OUDER 2017","FTE'S PERSONEN 0 - 0.5 FTE'S 2017","FTE'S PERSONEN 0.5 - 0.8 FTE'S 2017","FTE'S PERSONEN MEER DAN 0.8 FTE'S 2017","GEMIDDELDE FTE'S 2017","GEMIDDELDE LEEFTIJD 2017")]
Personeel2 <- Personeel2[Personeel2$ONDERWIJSTYPE=="BAO",]
Personeel2$ONDERWIJSTYPE <- NULL
Personeel2$ProcentVasteDienst <- Personeel2$`FTE'S PERSONEN IN VASTE DIENST 2018`/Personeel2$`FTE'S 2018`
names(Personeel2)[names(Personeel2) == "BRIN NUMMER"] <- "BRIN4"
for (i in (3:16)) {
  Personeel2[,i] <- Personeel2[,i]/Personeel2$`FTE'S 2018`
}

#Load and clean verzuimgetallen
VerzuimKengetallenPO <- read_excel("data/data_po/verzuimkengetallen-2015-2017.xlsx",sheet = "bestuur brin")
VerzuimKengetallenPO <- VerzuimKengetallenPO[,c("sector","bestuur","brin","VP17","MF17","GZD17")]
VerzuimKengetallenPO <- VerzuimKengetallenPO[VerzuimKengetallenPO$sector=="bo",]
names(VerzuimKengetallenPO)[names(VerzuimKengetallenPO) == "brin"] <- "BRIN4"
names(VerzuimKengetallenPO)[names(VerzuimKengetallenPO) == "VP17"] <- "VerzuimPercentage"
names(VerzuimKengetallenPO)[names(VerzuimKengetallenPO) == "MF17"] <- "MeldingsFrequentie"
names(VerzuimKengetallenPO)[names(VerzuimKengetallenPO) == "GZD17"] <- "GemiddeldeZiekteverzuimDuur"
VerzuimKengetallenPO <- VerzuimKengetallenPO[,c("BRIN4","VerzuimPercentage","MeldingsFrequentie","GemiddeldeZiekteverzuimDuur")]

#LEERLINGEN
#Load aantal leerlingen
Leerlingen <- read.csv("data/data_po/03-leerlingen-bo-sbo--leerjaar-geslacht-2017-2018.csv",sep = ";",stringsAsFactors = FALSE)
Leerlingen <- Leerlingen[,c("BRIN_NUMMER","VESTIGINGSNUMMER","GESLACHT","TOTAAL")]
names(Leerlingen)[names(Leerlingen) == "BRIN_NUMMER"] <- "BRIN4"
Leerlingen$BRIN6 <- paste(Leerlingen$BRIN4,0,Leerlingen$VESTIGINGSNUMMER,sep="")
Leerlingen <- cast(Leerlingen,BRIN6~GESLACHT,value = "TOTAAL",sum) #Determine total #leerlingen per BRIN6, so aggregate the different geslachten
Leerlingen$TOTAALLEERLINGEN <- Leerlingen$M+Leerlingen$O+Leerlingen$V
Leerlingen <- Leerlingen[,c("BRIN6","TOTAALLEERLINGEN")]

#Load gewicht
ImpulsGewicht <- read.csv("data/data_po/02-leerlingen-bo-swv-vestiging-gewicht-impulsgebied-schoolgewicht-2017-2018.csv",sep=";",stringsAsFactors = FALSE,dec = ",")
ImpulsGewicht <- ImpulsGewicht[,c("BRINNUMMER","VESTIGINGSNUMMER","PROVINCIE","IMPULSGEBIED","SCHOOLGEWICHT_VESTIGING","GEWICHT_0.0","GEWICHT_0.3","GEWICHT_1.2","TOTAAL")]
names(ImpulsGewicht)[names(ImpulsGewicht) == "BRINNUMMER"] <- "BRIN4"
ImpulsGewicht$BRIN6 <- paste(ImpulsGewicht$BRIN4,0,ImpulsGewicht$VESTIGINGSNUMMER,sep="")
ImpulsGewicht$TotaalLLGewicht <- ImpulsGewicht$GEWICHT_0.3*0.3+ImpulsGewicht$GEWICHT_1.2*1.2 #Determine total gewicht per BRIN6
ImpulsGewicht$GemiddeldGewicht <- ImpulsGewicht$TotaalLLGewicht/ImpulsGewicht$TOTAAL #Determine average gewicht per leerling per BRIN6
ImpulsGewicht$ProcentLLmetGewicht <- (ImpulsGewicht$GEWICHT_0.3+ImpulsGewicht$GEWICHT_1.2)/ImpulsGewicht$TOTAAL #Determine %leerlingen with gewicht per BRIN6
ImpulsGewicht <- ImpulsGewicht[,c("BRIN6","PROVINCIE","IMPULSGEBIED","GemiddeldGewicht","ProcentLLmetGewicht")]

#Load doublures
Doublures <- read.csv("data/data_po/09-leerlingen-bo-zittenblijven-2012-2018.csv",sep = ";",dec=",",stringsAsFactors = TRUE)
Doublures <- Doublures[,c("BRIN_NUMMER","VESTIGINGSNUMMER","ZITPERC_17")]
names(Doublures)[names(Doublures) == "BRIN_NUMMER"] <- "BRIN4"
Doublures$BRIN6 <- paste(Doublures$BRIN4,0,Doublures$VESTIGINGSNUMMER,sep="")
Doublures <- Doublures[, c("BRIN6", "ZITPERC_17")]

#Load wijkkarakteristieken from CBS (neighboorhood characteristics)
#First a code per gemeente
GemeenteCodes <- read_excel("data/data_po/Gemeenten alfabetisch 2019.xls")
GemeenteCodes <- GemeenteCodes[,c("GemeentecodeGM","Gemeentenaam")]
names(GemeenteCodes)[names(GemeenteCodes) == "GemeentecodeGM"] <- "GBD"

#Scores per gemeente
GemeenteScores <- read_excel("data/data_po/dimensiescore_gemeente (stand).xlsx")
GemeenteScores <- GemeenteScores[GemeenteScores$JAAR == 2016,]

#Scores ontwikkeling over jaren per gemeente
GemeenteOntwikkeling <- read_excel("data/data_po/dimensiescore_gemeente (ontwikkeling).xlsx")
GemeenteOntwikkeling <- GemeenteOntwikkeling[GemeenteOntwikkeling$BEGIN==2012&GemeenteOntwikkeling$EINDE==2016,]

#Merge together for the scores per gemeente
Gemeente <- GemeenteCodes
Gemeente <- merge(Gemeente,GemeenteScores)
Gemeente <- merge(Gemeente,GemeenteOntwikkeling)
Gemeente <- subset(Gemeente, select = -c(GBD,JAAR,BEGIN,EINDE))
Gemeente$Gemeentenaam <- toupper(Gemeente$Gemeentenaam)
names(Gemeente)[names(Gemeente)=="Gemeentenaam"]<-"GEMEENTENAAM"

#Determine krimp/groei per school region
Krimpregio <- read_excel("data/data_po/08.-leerlingen-bo-en-sbo-groei-en-krimp-aantal-leerlingen-2012-2036.xlsx",skip = 19)
Krimpregio <- Krimpregio[,c("ONDERWIJSSOORT","VESTIGINGSNUMMER","GEMEENTENAAM VESTIGING","Procentuele verandering 2016-2021...16")]
names(Krimpregio)[names(Krimpregio) == "Procentuele verandering 2016-2021...16"] <- "GroeiLeerlingenGemeente20162021"
names(Krimpregio)[names(Krimpregio) == "VESTIGINGSNUMMER"] <- "BRIN6"
Krimpregio$BRIN4 <- substr(Krimpregio$BRIN6,1,4)
Krimpregio <- Krimpregio[Krimpregio$ONDERWIJSSOORT=="Regulier Basisonderwijs (BO)",]
Krimpregio$krimp <- ifelse(Krimpregio$GroeiLeerlingenGemeente20162021< -0.075,TRUE,FALSE) #Based on definition from CBS
Krimpregio <- Krimpregio[,c('BRIN6', "GroeiLeerlingenGemeente20162021", 'krimp')]

# FINANCIALS - overheidsbekostiging per school
#Personele bekostiging
PersoneleBekostiging <- read_excel("data/data_po/03-personele-bekostiging-bo-2017-2018.xls") #make sure it is the right sheet if we copy to VO
PersoneleBekostiging <- PersoneleBekostiging[,c("Brin","TOTAAL_AANTAL_LEERLINGEN","SCHOOLGEWICHT","AANTAL_GEWICHTENLEERLINGEN_IN_IMPULSGEBIED","IMPULSGEBIEDEN","TOTAAL")]
names(PersoneleBekostiging)[names(PersoneleBekostiging) == "Brin"] <- "BRIN4"
names(PersoneleBekostiging)[names(PersoneleBekostiging) == "TOTAAL"] <- "TotaalDeelPersoneleBekostiging"
PersoneleBekostiging <- PersoneleBekostiging[,c("BRIN4","TOTAAL_AANTAL_LEERLINGEN","TotaalDeelPersoneleBekostiging")]
names(PersoneleBekostiging)[names(PersoneleBekostiging) == "TOTAAL_AANTAL_LEERLINGEN"] <- "AantalLLvolgensPersBegr"
PersoneleBekostiging$PersBekostperLL <- PersoneleBekostiging$TotaalDeelPersoneleBekostiging/PersoneleBekostiging$`AantalLLvolgensPersBegr`
PersoneleBekostiging <- PersoneleBekostiging[,c('BRIN4', "PersBekostperLL")]

#Materiele instandhouding
MaterieleInstandhouding <- read_excel("data/data_po/01-materiele-instandhouding-bo-2018.xls")
MaterieleInstandhouding <- MaterieleInstandhouding[,c("Brin","TOTAAL_AANTAL_LEERLINGEN","TOTAAL_MI")]
names(MaterieleInstandhouding)[names(MaterieleInstandhouding) == "Brin"] <- "BRIN4"
names(MaterieleInstandhouding)[names(MaterieleInstandhouding) == "TOTAAL_MI"] <- "TotaalDeelMaterieleInstandhouding"
names(MaterieleInstandhouding)[names(MaterieleInstandhouding) == "TOTAAL_AANTAL_LEERLINGEN"] <- "Leerlingen materiele instandhouding"
MaterieleInstandhouding$MatInstandperLL <- MaterieleInstandhouding$TotaalDeelMaterieleInstandhouding/MaterieleInstandhouding$`Leerlingen materiele instandhouding`
MaterieleInstandhouding <- MaterieleInstandhouding[,c('BRIN4', "MatInstandperLL")]

# FINANCIALS - financieel jaarverslag op bestuursniveau
#Financiele kengetallen
FinancieleKengetallen <- read_excel("data/data_po/16-financiele-kengetallen-per-bestuur-2013-2017.xlsx",sheet = " Kengetallen")
FinancieleKengetallen <- FinancieleKengetallen[,c("Bevoegd Gezag","Beleidsterrein","Jaar","Huisvestingsratio","Liquiditeit (current ratio)","Liquiditeit (quick ratio)","Personeelslasten / rijksbijdragen","Personeelslasten / totaal baten plus financiële baten","Personeelslasten / totaal lasten plus financiële lasten","Rentabiliteit","Solvabiliteit I","Solvabiliteit II","Weerstandsvermogen","Kapitalisatiefactor","Weerstandsvermogen exclusief materiële vaste activa","Algemene reserve / totaal baten plus financiële baten","Voorzieningen / totaal baten plus financiële baten","Rijksbijdragen / totaal baten plus financiële baten","Overige overheidsbijdragen en -subsidies / totaal baten plus financiële baten","Investeringen huisvesting / totaal baten plus financiële baten","Investeringen inventaris en apparatuur / totaal baten plus financiële baten","Netto werkkapitaal","Beleggingen ten opzichte van het eigen vermogen","Contractactiviteiten / Rijksbijdragen", "Contractactiviteiten / totaal baten plus financiële baten")]
FinancieleKengetallen[is.na(FinancieleKengetallen)] <- 9999
warning("Warning: Setting all financiele kengetallen that are NA to 9999") #As the Random Forest model is non-linear, this is best to make sure the results are not affected
names(FinancieleKengetallen)[names(FinancieleKengetallen) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"

#Overheidsbijdragen (other than only lumpsum)
Overheidsbijdragen <- read_excel("data/data_po/12-overheidsbijdragen-2013-2017.xlsx")
Overheidsbijdragen <- Overheidsbijdragen[Overheidsbijdragen$Beleidsterrein == "PO",]
Overheidsbijdragen <- Overheidsbijdragen[Overheidsbijdragen$Jaar == 2017,]
names(Overheidsbijdragen)[names(Overheidsbijdragen) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
Overheidsbijdragen[is.na(Overheidsbijdragen)] <- 0 #We assume that if there is no number in the jaarverslag, it is 0
warning("Warning: Setting all overheidsbijdragen that are NA to 0")

#Overige baten
OverigeBaten <- read_excel("data/data_po/13-overige-baten-2013-2017.xlsx")
OverigeBaten <- OverigeBaten[OverigeBaten$Beleidsterrein == "PO",]
names(OverigeBaten)[names(OverigeBaten) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
OverigeBaten <- OverigeBaten[OverigeBaten$Jaar == 2017,]
OverigeBaten[is.na(OverigeBaten)] <- 0 #We assume that if there is no number in the jaarverslag, it is 0

#Lasten
Lasten <- read_excel("data/data_po/14-lasten-2013-2017.xlsx")
Lasten <- Lasten[Lasten$Beleidsterrein == "PO",]
names(Lasten)[names(Lasten) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
Lasten <- Lasten[Lasten$Jaar == 2017,]
Lasten[is.na(Lasten)] <- 0 #We assume that if there is no number in the jaarverslag, it is 0

#Merge an financials together on board level
FinancienBestuur <- merge(FinancieleKengetallen,Overheidsbijdragen)
FinancienBestuur <- merge(FinancienBestuur,OverigeBaten)
FinancienBestuur <- merge(FinancienBestuur,Lasten)
FinancienBestuur$Beleidsterrein<-NULL
FinancienBestuur$Jaar<-NULL
FinancienBestuur$Groepering<-NULL
FinancienBestuur$`Naam Kort`<-NULL

# Drop all financials that are not taken into the analysis
FinancienBestuur$`Personeelslasten / rijksbijdragen` <- NULL
FinancienBestuur$`Personeelslasten / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Algemene reserve / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Voorzieningen / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Rijksbijdragen / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Overige overheidsbijdragen en -subsidies / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Investeringen huisvesting / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Investeringen inventaris en apparatuur / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Contractactiviteiten / Rijksbijdragen` <- NULL
FinancienBestuur$`Contractactiviteiten / totaal baten plus financiële baten` <- NULL
FinancienBestuur$`Rijksbijdragen OCW` <- NULL
FinancienBestuur$`Rijksbijdragen EZ` <- NULL
FinancienBestuur$`Rijksbijdragen OCW/EZ` <- NULL
FinancienBestuur$`Rijksbijdrage academische ziekenhuizen` <- NULL
FinancienBestuur$`Doorbetalingen rijksbijdrage SWV` <- NULL
FinancienBestuur$`Inkomensoverdracht van rijksbijdragen` <- NULL
FinancienBestuur$`Ontvangen doorbetalingen Rijksbijdrage SWV` <- NULL
FinancienBestuur$`Overige rijksbijdragen` <- NULL
FinancienBestuur$Rijksbijdragen <- NULL

survey_BG <- read.csv("data/data_po/survey_BG.csv")
survey_BG <- survey_BG[,-c(1)]
survey_BRIN6 <- read.csv("data/data_po/survey_BRIN6.csv")

######## Create Master data/data_po file with all data/data_po columns for PO ########
data_PO <- BRIN6RowsPO
data_PO <- merge(data_PO, scores_PO_VA, by='BRIN6', all.x = TRUE)

data_PO <- merge(data_PO, Personeel, by='BRIN4', all.x = TRUE)
data_PO <- merge(data_PO, Personeel2, by='BRIN4', all.x = TRUE)
data_PO <- merge(data_PO, VerzuimKengetallenPO, by='BRIN4', all.x=TRUE)

data_PO <- merge(data_PO, Leerlingen, by='BRIN6', all.x = TRUE)
data_PO <- merge(data_PO, ImpulsGewicht, by='BRIN6', all.x = TRUE)
data_PO <- merge(data_PO, Doublures, by= "BRIN6", all.x = TRUE)

data_PO <- merge(data_PO, Gemeente, by='GEMEENTENAAM', all.x = TRUE)
data_PO <- merge(data_PO, Krimpregio, by='BRIN6', all.x=TRUE)

data_PO <- merge(data_PO, FinancienBestuur, by='BEVOEGD.GEZAG.NUMMER', all.x=TRUE)

LLN_per_BG <- data_PO %>%
  group_by(BEVOEGD.GEZAG.NUMMER) %>%
  summarise(LLN_per_BG = sum(TOTAALLEERLINGEN, na.rm = TRUE))

data_PO <- merge(data_PO, LLN_per_BG, by='BEVOEGD.GEZAG.NUMMER', all.x = TRUE)


#######################
#Determine the baten per LL
for (i in (grep("^Overige subsidies OCW$", colnames(data_PO)):grep("^Overige baten$", colnames(data_PO)))) {
  data_PO[,i] <- data_PO[,i]/data_PO$LLN_per_BG
}

####################################
#Merge also the survey data/data_po
data_PO <- merge(data_PO, survey_BG, by="BEVOEGD.GEZAG.NUMMER", all.x = TRUE)
data_PO <- merge(data_PO, survey_BRIN6, by="BRIN6", all.x = TRUE)
summary(data_PO$director_exists)

################################ ANALYSIS ####################################################
#Final cleaning of column names and format
data_PO$DENOMINATIE<-as.factor(data_PO$DENOMINATIE)
data_PO$PROVINCIE<- as.factor(data_PO$PROVINCIE)
names(data_PO)<-str_replace_all(names(data_PO), c(" " = "." , "," = "" ))
names(data_PO)<-str_replace_all(names(data_PO), c("'" = "_" , "," = "" ))
names(data_PO)<-str_replace_all(names(data_PO), c("[(]" = "_" , "," = "" ))
names(data_PO)<-str_replace_all(names(data_PO), c("[)]" = "_" , "," = "" ))
names(data_PO)<-str_replace_all(names(data_PO), c("/" = "_" , "," = "" ))
names(data_PO)<-str_replace_all(names(data_PO), c("-" = "_" , "," = "" ))
names(data_PO)[names(data_PO) == "Onderwijsondersteunend.personeel.(OOP/OBP)"] <- "Onderwijsondersteunend.personeel"

####### De-correlate VA & verwachte score
model <- lm(VA ~ StdSchoolweging, data=data_PO, na.action = na.exclude)
data_PO$VA <- data_PO$VA - (model$coefficients[1]+data_PO$StdSchoolweging*model$coefficients[2])
scatter.smooth(data_PO$VA, data_PO$StdSchoolweging)

# assign percentile socres
data_PO$percentiel <- ntile(data_PO["VA"],100)
data_PO$top_bottom <- ifelse((data_PO$VA>0.5)&(data_PO$range<1),
                                            "top",
                                            ifelse((data_PO$VA< -0.5)&(data_PO$range<1),
                                                   "bottom"," "))
data_PO <- data_PO%>%select(-bm_exists,everything())
data_PO <- data_PO%>%select(-hr_exists,everything())
data_PO <- data_PO%>%select(-finance_exists,everything())

## Add group and national means for dashboard
data_PO_DB <- data_PO[, c("BRIN6", "bm_exists", "hr_exists", "finance_exists", "director_exists", "teacher_exists",
                          "ZITPERC_17",
                          "LLN_per_BG",
                          "Liquiditeit._current.ratio_", "Liquiditeit._quick.ratio_",
                          "VA",
                          "TOTAALLEERLINGEN",
                          "BRIN6_per_BG",
                          "StdSchoolweging",
                          "GroeiLeerlingenGemeente20162021")]

#- Creat customized variable regarding board size
data_PO_DB$board_size <- ifelse(data_PO_DB$BRIN6_per_BG ==1, "A",
                             ifelse(data_PO_DB$BRIN6_per_BG >1  & data_PO_DB$BRIN6_per_BG <=5  , "B",
                                    ifelse(data_PO_DB$BRIN6_per_BG >5  & data_PO_DB$BRIN6_per_BG <=20  , "C","D")))
#- Creat customized variable regarding social economic indicator
data_PO_DB$soeco_indicator <- ifelse(data_PO_DB$StdSchoolweging<= -0.5 , "A",
                                  ifelse(data_PO_DB$StdSchoolweging> -0.5 & data_PO_DB$StdSchoolweging <= 0 , "B",
                                         ifelse(data_PO_DB$StdSchoolweging > 0 & data_PO_DB$StdSchoolweging <= 0.5, "C","D")))
#- Get top 10% and lower 10% cut-offs of Pupil growth Groeileerlingen
p90 <- as.numeric(quantile(data_PO_DB$GroeiLeerlingenGemeente20162021,probs = .9,na.rm = TRUE))
p10 <- as.numeric(quantile(data_PO_DB$GroeiLeerlingenGemeente20162021,probs = .1,na.rm = TRUE))

#- Creat customized variable regarding extremes of Pupil growth Groeileerlingen
data_PO_DB$pup_gwth <- ifelse(data_PO_DB$GroeiLeerlingenGemeente20162021 <= p10 , "A",
                           ifelse(data_PO_DB$GroeiLeerlingenGemeente20162021 > p10 & data_PO_DB$GroeiLeerlingenGemeente20162021 < p90  , "B","C"))

# Take average by group
data_PO_DB_grouped <- data_PO_DB %>%select(-c(BRIN6,bm_exists, hr_exists, finance_exists, director_exists, teacher_exists, GroeiLeerlingenGemeente20162021 ))%>%
  group_by( board_size,soeco_indicator,pup_gwth) %>% summarise_all(funs(mean))%>% as.data.frame()

### Save data/data_poset here

write.csv("data/data_po/Masterfiles/data_PO_all.csv")

