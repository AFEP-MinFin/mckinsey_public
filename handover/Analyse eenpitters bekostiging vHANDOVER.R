###For Standaardnormaal analyse
####PREPARE R AND LOAD PACKAGES####
## verwijderd privacy

library("readxl")
library("reshape")
library("dplyr")

#Lasten (expenses) can be analyzed for each column in the DUO-dataset independently or can be categorized
CategorizeCosts <- TRUE

#Set the percentage that should be top / bottom
RankPercentage <- 33

#Load dataset for the laster (expenses)
Lasten <- read_excel("Data/14-lasten-2013-2017.xlsx")

#Select PO and clean column names
Lasten <- Lasten[Lasten$Beleidsterrein %in% c("PO"),]
names(Lasten)[names(Lasten) == "Bevoegd Gezag"] <- "BEVOEGD.GEZAG.NUMMER"
Lasten <- Lasten[Lasten$Jaar == 2017,]
Lasten$Beleidsterrein <- NULL
Lasten$Groepering <- NULL
Lasten$`Naam Kort` <- NULL
Lasten$Jaar <- NULL

#Make all NAs zero (zero lasten)
Lasten[is.na(Lasten)] <- 0

#Determine the columns that contain aggregates
colnames(Lasten) <- ifelse(colnames(Lasten) %in% c(
  "Lonen, salarissen, sociale lasten en pensioenlasten",
  "Overige personele lasten",
  "Uitkeringen die personeelslasten verminderen",
  "Personeelslasten",
  "Afschrijvingen",
  "Huisvestingslasten",
  "Overige lasten",
  "Accountantshonoraria"
),paste("Aggregate_",colnames(Lasten),sep="_"),colnames(Lasten))

#Remove columns that are all zero
Lasten <- Lasten[,apply(Lasten,2,function(x) !all(x==0))]

#Categorize by categories
if(CategorizeCosts){
  CategorizedColumns <- c(
    "Aggregate__Personeelslasten",
    "Aggregate__Huisvestingslasten",
    "Aggregate__Afschrijvingen",
    "Administratie- en beheerslasten",
    "Inventaris en apparatuur", #Wordt samengevoegd met leer en hulpmiddelen
    "Leer- en hulpmiddelen", #Wordt samengevoegd met inventaris en apparatuur
    "Dotatie overige voorzieningen", #Wordt overige
    "Overige (overige overige lasten)", #Wordt overige
    "Aggregate__Accountantshonoraria" #Wordt overige
  )
  Lasten <- Lasten[,c("BEVOEGD.GEZAG.NUMMER",CategorizedColumns)]

  #Bundle inventaris & apparatuur and leer & hulpmiddelen
  Lasten$InventarisApparatuurLeerHulpmiddelen <- Lasten$`Inventaris en apparatuur` + Lasten$`Leer- en hulpmiddelen`
  Lasten$`Inventaris en apparatuur` <- NULL
  Lasten$`Leer- en hulpmiddelen` <- NULL

  #Bundle 'other' costs
  Lasten$Overige <- Lasten$`Dotatie overige voorzieningen`+Lasten$`Overige (overige overige lasten)`+Lasten$Aggregate__Accountantshonoraria
  Lasten$`Dotatie overige voorzieningen` <- NULL
  Lasten$`Overige (overige overige lasten)` <- NULL
  Lasten$Aggregate__Accountantshonoraria <- NULL

  #Select columns and determine total
  Lasten <- Lasten[,c("BEVOEGD.GEZAG.NUMMER","Aggregate__Personeelslasten","Aggregate__Huisvestingslasten","InventarisApparatuurLeerHulpmiddelen","Administratie- en beheerslasten","Aggregate__Afschrijvingen","Overige")]
  Lasten$Totaal <- Lasten$Aggregate__Personeelslasten + Lasten$Aggregate__Huisvestingslasten+Lasten$InventarisApparatuurLeerHulpmiddelen+Lasten$`Administratie- en beheerslasten`+Lasten$Aggregate__Afschrijvingen+Lasten$Overige
}

FinancienBestuur <- Lasten

#####SELECT ONLY 1-PITTERS####
#Load all PO schools
BRIN6RowsAllPO <- read.csv("Data/03-alle-vestigingen-bo-adres.csv",sep = ";",stringsAsFactors = FALSE)
BRIN6RowsPO <- BRIN6RowsAllPO[,c("BEVOEGD.GEZAG.NUMMER","BRIN.NUMMER","VESTIGINGSNUMMER","VESTIGINGSNAAM","PLAATSNAAM","GEMEENTENAAM","DENOMINATIE")] #We could choose to add additional region indicators if interesting

#Select eenpitters PO
BRIN6RowsPO$Eenpitter <- !(BRIN6RowsPO$BEVOEGD.GEZAG.NUMMER %in% BRIN6RowsPO$BEVOEGD.GEZAG.NUMMER[duplicated(BRIN6RowsPO$BEVOEGD.GEZAG.NUMMER)])
EenpittersPO <- BRIN6RowsPO[BRIN6RowsPO$Eenpitter,]
names(EenpittersPO)[names(EenpittersPO) == "BRIN.NUMMER"] <- "BRIN4"
EenpittersPO <- EenpittersPO[,c("BEVOEGD.GEZAG.NUMMER","BRIN4")]

#Load VA data from PO complete
data_PO <- read.csv("Data/Masterfiles/data_PO_all.csv")
data_PO <- data_PO[,-c(1)]
scores_PO <- data_PO[,c("BRIN6","VA","range","IMPULSGEBIED","GroeiLeerlingenGemeente20162021","TOTAALLEERLINGEN")]

#Merge the financials and value-added
EenpittersPO <- merge(EenpittersPO,FinancienBestuur)
scores_PO$BRIN4 <- substr(scores_PO$BRIN6,1,4)
EenpittersPO <- merge(EenpittersPO,scores_PO,by="BRIN4")
EenpittersPO <- unique(EenpittersPO)
EenpittersPO <- EenpittersPO[,colSums(!is.na(EenpittersPO) & (EenpittersPO==0))<nrow(EenpittersPO)]

#Remove columns that only contain zeros
EenpittersPO <- EenpittersPO[,apply(EenpittersPO,2,function(x) !all(x==0))]

#Remove columns for which we do not know number of leerlingen
EenpittersPO <- EenpittersPO[!is.na(EenpittersPO$TOTAALLEERLINGEN),]

EenpittersPO$GroeiKrimp <- ifelse(EenpittersPO$GroeiLeerlingenGemeente20162021 < -0.075,"Krimp",ifelse(EenpittersPO$GroeiLeerlingenGemeente20162021 >0,"Groei",""))

#Divide the financial numbers by the number of leerlingen tot know the average expenses per leerling
NonFinancialColumns <- c("BRIN4","BEVOEGD.GEZAG.NUMMER","Percentieladjusted","IMPULSGEBIED",
                         "GroeiLeerlingenGemeente20162021","BRIN6","VA","GroeiKrimp","range","TOTAALLEERLINGEN","Rank")
for(Lastencategorie in colnames(EenpittersPO)[!(colnames(EenpittersPO) %in% NonFinancialColumns)]){
  EenpittersPO[[Lastencategorie]] <- EenpittersPO[[Lastencategorie]]/EenpittersPO$TOTAALLEERLINGEN
}

#Take only scholen between 150 and 250 leerlingen
EenpittersPOSize <- EenpittersPO[EenpittersPO$TOTAALLEERLINGEN > 150 & EenpittersPO$TOTAALLEERLINGEN < 250,]
#Not the 3 highest and lowest outliers
EenpittersPOSize <- EenpittersPOSize[EenpittersPOSize$Totaal > sort(EenpittersPOSize$Totaal,decreasing = FALSE)[3] & EenpittersPOSize$Totaal < sort(EenpittersPOSize$Totaal,decreasing = TRUE)[3],]
OverallMeanNew <- data.frame(apply(EenpittersPOSize[,(!(colnames(EenpittersPOSize)%in%NonFinancialColumns))], MARGIN=c(2), mean))
colnames(OverallMeanNew) <- "OverallMean"

#Remove eenpitters for which we do not know the VA
EenpittersPOSize <- EenpittersPOSize[!is.na(EenpittersPOSize$VA),]

#Determine the top and bottom value added performers
cutoff = 0.33
Bottom <- quantile(EenpittersPOSize$VA,cutoff,na.rm = TRUE)
Top <- quantile(EenpittersPOSize$VA,1-cutoff,na.rm = TRUE)
EenpittersPOSize$Rank <- ifelse(EenpittersPOSize$VA >= Top,"top",ifelse(EenpittersPOSize$VA <= Bottom,"bottom",""))

TopEenpitters <- EenpittersPOSize[EenpittersPOSize$Rank == "top" & !is.na(EenpittersPOSize$Rank),]
BottomEenpitters <- EenpittersPOSize[EenpittersPOSize$Rank == "bottom" & !is.na(EenpittersPOSize$Rank),]

NonFinancialColumns <- c("BRIN4","BEVOEGD.GEZAG.NUMMER","Percentieladjusted","IMPULSGEBIED",
                         "GroeiLeerlingenGemeente20162021","BRIN6","VA","GroeiKrimp","TOTAALLEERLINGEN","range","Rank")

#Determine top and bottom averages for the cost categories
TopMean <- data.frame(apply(TopEenpitters[,(!(colnames(TopEenpitters)%in%NonFinancialColumns))], MARGIN=c(2), mean))
colnames(TopMean) <- "TopMean"

BottomMean <- data.frame(apply(BottomEenpitters[,(!(colnames(BottomEenpitters)%in%NonFinancialColumns))], MARGIN=c(2), mean))
colnames(BottomMean) <- "BottomMean"
