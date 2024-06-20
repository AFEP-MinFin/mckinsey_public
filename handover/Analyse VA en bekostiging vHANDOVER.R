#This script is used to make the analyses of relation between vlaue-added and financing (bekostiging) as shown in 'scatter plots' in the report

## setwd verwijderd privacy

library("readxl")
library("reshape")
library("dplyr")
library("pdp")
library("vip")
library("Hmisc")
library("Benchmarking")
library("swfscMisc")
library("stringr")
library("tidyverse")
library("vioplot")
library("ggplot2")

#Parameters: growth over 5 years for definition of krimp and groei
KrimpGebied <- -0.075
GroeiGebied <- 0

### Keep range for funding per student at 4000-15000: this keeps axes comparable in figures

####PO####
#Read bekostiging per leerling
BekostigingLeerling <- read_excel("Data/OCW Bekostiging per leerling PO.xlsx")
names(BekostigingLeerling)[names(BekostigingLeerling) == "...10"] <- "Bekostiging_per_Leerling"
names(BekostigingLeerling)[names(BekostigingLeerling) == "BRINNUMMER"] <- "BRIN4"

data_PO <- read.csv("Data/Masterfiles/data_PO_all.csv")
data_PO_BRIN4 <- data_PO[data_PO$BRIN6_per_BRIN4 == 1,]

data_PO_bekostiging <- merge(data_PO_BRIN4,BekostigingLeerling,by="BRIN4")
data_PO_bekostiging <- data_PO_bekostiging[,c("BRIN4","Bekostiging_per_Leerling","Totaal_leerlingen","VA","GroeiLeerlingenGemeente20162021","BRIN6_per_BG","IMPULSGEBIED","DENOMINATIE")]

#Determine the different segments:
data_PO_bekostiging$KrimpGroei <- ifelse(data_PO_bekostiging$GroeiLeerlingenGemeente20162021 < KrimpGebied, "Krimp",ifelse(data_PO_bekostiging$GroeiLeerlingenGemeente20162021 > GroeiGebied,"Groei",""))
data_PO_bekostiging$Bestuursgrootte <- ifelse(data_PO_bekostiging$BRIN6_per_BG <= 5,"Klein_Bestuur",ifelse(data_PO_bekostiging$BRIN6_per_BG <= 20,"Gemiddeld_Bestuur","Groot_Bestuur"))
data_PO_bekostiging$Impuls <- ifelse(data_PO_bekostiging$IMPULSGEBIED == 1,"Impulsgebied","Niet_impulsgebied")
data_PO_bekostiging$Schoolgrootte <- ifelse(data_PO_bekostiging$Totaal_leerlingen < 50,"Heel_Klein",ifelse(data_PO_bekostiging$Totaal_leerlingen < 200,"Klein","Groot"))
data_PO_bekostiging$OpenbaarBijzonder <- ifelse(data_PO_bekostiging$DENOMINATIE == "Openbaar","Openbaar","Bijzonder")

#Remove outliers >15.000 per leerling (also for the averages)
data_PO_bekostiging <- data_PO_bekostiging[data_PO_bekostiging$Bekostiging_per_Leerling < 15000,]
plot(data_PO_bekostiging$Bekostiging_per_Leerling,data_PO_bekostiging$VA,xlim=c(4000,15000))

#Write overall plot of bekostiging vs VA
data_PO_bekostiging$Colour="black"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,data_PO_bekostiging$VA,main = "All",xlim = c(4000,15000))

#Adjust coloring based on segment (different colors in final report)
# Create new column filled with default colour
data_PO_bekostiging$Colour="grey"
# Set new column values to appropriate colours
data_PO_bekostiging$Colour[data_PO_bekostiging$KrimpGroei == "Krimp"] <- "blue"
data_PO_bekostiging$Colour[data_PO_bekostiging$KrimpGroei == "Groei"] <- "red"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,col=data_PO_bekostiging$Colour,data_PO_bekostiging$VA,main = "KostenVA_KrimpGroei",xlim = c(4000,15000))
mean(data_PO_bekostiging$Bekostiging_per_Leerling[data_PO_bekostiging$KrimpGroei == "Krimp"],na.rm = TRUE)
mean(data_PO_bekostiging$Bekostiging_per_Leerling[data_PO_bekostiging$KrimpGroei == "Groei"],na.rm = TRUE)

#Variation for impuls and niet-impuls
data_PO_bekostiging$Colour="yellow"
# Set new column values to appropriate colours
data_PO_bekostiging$Colour[data_PO_bekostiging$Impuls == "Impulsgebied"] <- "red"
data_PO_bekostiging$Colour[data_PO_bekostiging$Impuls == "Niet_impulsgebied"] <- "blue"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,col=data_PO_bekostiging$Colour,data_PO_bekostiging$VA,main = "KostenVA_Impuls",xlim = c(4000,15000))

#Variation for school size
data_PO_bekostiging$Colour="yellow"
# Set new column values to appropriate colours
data_PO_bekostiging$Colour[data_PO_bekostiging$Schoolgrootte == "Heel_Klein"] <- "red"
data_PO_bekostiging$Colour[data_PO_bekostiging$Schoolgrootte == "Klein"] <- "blue"
data_PO_bekostiging$Colour[data_PO_bekostiging$Schoolgrootte == "Groot"] <- "black"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,col=data_PO_bekostiging$Colour,data_PO_bekostiging$VA,main = "KostenVA_Schoolgrootte",xlim = c(4000,15000))

#Variation for board size
data_PO_bekostiging$Colour="yellow"
# Set new column values to appropriate colours
data_PO_bekostiging$Colour[data_PO_bekostiging$Bestuursgrootte == "Groot_Bestuur"] <- "red"
data_PO_bekostiging$Colour[data_PO_bekostiging$Bestuursgrootte == "Gemiddeld_Bestuur"] <- "blue"
data_PO_bekostiging$Colour[data_PO_bekostiging$Bestuursgrootte == "Klein_Bestuur"] <- "black"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,col=data_PO_bekostiging$Colour,data_PO_bekostiging$VA,main = "KostenVA_Bestuursgrootte",xlim = c(4000,15000))

#Variation for denomiatie (openbaar vs bijzonder)
data_PO_bekostiging$Colour="yellow"
# Set new column values to appropriate colours
data_PO_bekostiging$Colour[data_PO_bekostiging$OpenbaarBijzonder == "Openbaar"] <- "blue"
data_PO_bekostiging$Colour[data_PO_bekostiging$OpenbaarBijzonder == "Bijzonder"] <- "red"
plot(data_PO_bekostiging$Bekostiging_per_Leerling,col=data_PO_bekostiging$Colour,data_PO_bekostiging$VA,main = "KostenVA_Denominatie",xlim = c(4000,15000))

#####VO####
#Load VA
data_VO <- read.csv("Data/Masterfiles/data_VO_all.csv")

#Load bekostigingsdata
#Materiele bekostiging
BekostigingMaterieelVO <- read_excel("Data/Bekostiging VO (OCW)/Bekostiging materieel.xlsx",skip=4,sheet = "2018 def realisatie")
BekostigingMaterieelVO <- BekostigingMaterieelVO[,c("BRIN","TOTAAL","Totaal MI (excl. les, incl. lwoo pro)")]
BekostigingMaterieelVO$MatperLL <- BekostigingMaterieelVO$`Totaal MI (excl. les, incl. lwoo pro)`/BekostigingMaterieelVO$TOTAAL

#Personele bekostiging
BekostigingPersoneelVO <- read_excel("Data/Bekostiging VO (OCW)/Bekostiging personeel.xlsx",skip=3,sheet = "totaal 2018D")
BekostigingPersoneelVO <- BekostigingPersoneelVO[,c("BRIN","TOTAAL","Totaal incl. OND en nevenvestiging")]
BekostigingPersoneelVO$`Totaal incl. OND en nevenvestiging` <- as.double(BekostigingPersoneelVO$`Totaal incl. OND en nevenvestiging`)
BekostigingPersoneelVO$TOTAAL <- as.double(BekostigingPersoneelVO$TOTAAL)
BekostigingPersoneelVO$PersperLL <- BekostigingPersoneelVO$`Totaal incl. OND en nevenvestiging`/BekostigingPersoneelVO$TOTAAL

#Determine total bekostiging per leerling
BekostigingVO <- merge(BekostigingMaterieelVO,BekostigingPersoneelVO,by="BRIN")
BekostigingVO$BekostigingperLL <- BekostigingVO$MatperLL+BekostigingVO$PersperLL
names(BekostigingVO)[names(BekostigingVO) == "BRIN"] <- "BRIN4"

#Test relationship
data_VO <- merge(data_VO,BekostigingVO,by="BRIN4",all.x=TRUE)
#Remove outliers >15.000 per leerling (also for the averages)
data_VO <- data_VO[data_VO$BekostigingperLL < 15000,]
plot(data_VO$BekostigingperLL,data_VO$VA,xlim=c(4000,15000))

plot(data_VO$BekostigingperLL,data_VO$VA,xlim = c(4000,15000))
cor.test(data_VO$BekostigingperLL,data_VO$VA)
summary(lm(data_VO$BekostigingperLL~data_VO$VA))
data_VO$Colour <- "black"
plot(data_VO$BekostigingperLL,data_VO$VA,main = "VO",xlim = c(4000,15000))

#Determine the different segments:
data_VO$KrimpGroei <- ifelse(data_VO$GroeiLeerlingenGemeente20162021 < KrimpGebied, "Krimp",ifelse(data_VO$GroeiLeerlingenGemeente20162021 > GroeiGebied,"Groei",""))
data_VO$Bestuursgrootte <- ifelse(data_VO$BRIN6_per_BG <= 5,"Klein_Bestuur","Groot_Bestuur")
data_VO$Impuls <- ifelse(data_VO$IMPULSGEBIED == 1,"Impulsgebied","Niet_impulsgebied")
data_VO$Schoolgrootte <- ifelse(data_VO$TOTAAL < 800,"Heel_Klein",ifelse(data_VO$TOTAAL < 1200,"Klein","Groot"))
#Merge denominatie
Denominaties <- read.csv("Data/02-alle-vestigingen-vo.csv",sep = ";")
Denominaties <- Denominaties[,c("VESTIGINGSNUMMER","DENOMINATIE")]
colnames(Denominaties) <- c("BRIN6","DENOMINATIE")
data_VO <- merge(data_VO,Denominaties,by="BRIN6",all.x=TRUE)
data_VO$OpenbaarBijzonder <- ifelse(data_VO$DENOMINATIE == "Openbaar","Openbaar","Bijzonder")

#Adjust coloring based on segment
# Create new column filled with default colour

#Analysis krimp / groei
data_VO$Colour="grey"
# Set new column values to appropriate colours
data_VO$Colour[data_VO$KrimpGroei == "Krimp"] <- "blue"
data_VO$Colour[data_VO$KrimpGroei == "Groei"] <- "red"
plot(data_VO$BekostigingperLL,data_VO$VA,col=data_VO$Colour,main = "KostenVA_VO_Krimp",xlim = c(4000,15000))

#Analysis impuls / niet-impuls
data_VO$Colour="yellow"
# Set new column values to appropriate colours
data_VO$Colour[data_VO$Impuls == "Impulsgebied"] <- "red"
data_VO$Colour[data_VO$Impuls == "Niet_impulsgebied"] <- "blue"
plot(data_VO$BekostigingperLL,data_VO$VA,col=data_VO$Colour,main = "KostenVA_VO_Impuls",xlim = c(4000,15000))

#Analysis school size
data_VO$Colour="grey"
# Set new column values to appropriate colours
data_VO$Colour[data_VO$Schoolgrootte == "Heel_Klein"] <- "red"
data_VO$Colour[data_VO$Schoolgrootte == "Klein"] <- "blue"
data_VO$Colour[data_VO$Schoolgrootte == "Groot"] <- "black"
plot(data_VO$BekostigingperLL,data_VO$VA,col=data_VO$Colour,main = "KostenVA_VO_Schoolgrootte",xlim = c(4000,15000))

#Analysis board size
data_VO$Colour="yellow"
# Set new column values to appropriate colours
data_VO$Colour[data_VO$Bestuursgrootte == "Groot_Bestuur"] <- "red"
data_VO$Colour[data_VO$Bestuursgrootte == "Klein_Bestuur"] <- "blue"
plot(data_VO$BekostigingperLL,data_VO$VA,col=data_VO$Colour,main = "KostenVA_VO_Bestuursgrootte",xlim = c(4000,15000))

#Analysis board size (openbaar vs bijzonder)
data_VO$Colour="yellow"
# Set new column values to appropriate colours
data_VO$Colour[data_VO$OpenbaarBijzonder == "Openbaar"] <- "blue"
data_VO$Colour[data_VO$OpenbaarBijzonder == "Bijzonder"] <- "red"
plot(data_VO$BekostigingperLL,data_VO$VA,col=data_VO$Colour,main = "KostenVA_VO_Denominatie",xlim = c(4000,15000))
