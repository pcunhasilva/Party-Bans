# Party Bans
# Patrick C. Silva, William O'Brochta, and Luwei Ying

rm(list = ls())
# Load Libraries
library(plyr)
library(data.table)
library(readxl)
library(foreign)
library(readstata13)
library(stringi)
library(plm)
library(car)
library(dplyr)
library(stargazer)
library(countrycode)

# Set Directory:
wd <- "~/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/"
setwd(wd)
# setwd("/Users/luweiying/Desktop/Comparative Party Politics/Party_Bans/DataAnalysis")

###############################
############ V-Dem ############
###############################

# Read Data
data_vdem <- read.csv("V-Dem/V-Dem-DS-CY+Others-v6.2.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# V-Dem dataset 

# Keep only observations after 1959
smalldata_vdem <- subset(data_vdem, subset = year>1959)

#### Keep only variables of interest:
varKeep <- c("country_name", "country_id", "country_text_id", "year", "COWcode",
             "v2psparban_ord", "e_reserves_billions", "e_population",
             "e_polity2") 
smalldata_vdem <- smalldata_vdem[, varKeep]

#### Rename variables
names(smalldata_vdem) <- c("country_name", "country_id", "country_abb", "year",
                           "cow_code", "party_banVD", "oilHM", "popHM", "polity2")

#### Recode variables
# Party Bans
smalldata_vdem$party_banVD <- recode(smalldata_vdem$party_banVD, "0:3=1;else = 0")


###################################################
############ Quality of the Government ############
###################################################

# Read Data
data_qgov <- read.csv("QoG/qog_std_ts_jan17.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# As V-Dem, QoG dataset also contains variables from other datasets that we are interested. 
# Our main goal with this dataset is to recover different country codes.

# Keep variables of interest
varKeep <- c("ccode", "cname", "year", "ccodealp","ccodecow", "ccodewb", "chga_demo", "ht_region") 

# Remove variables and save the final version
smalldata_qgov <- data_qgov[,varKeep]

# Rename variables
names(smalldata_qgov) <- c("ccode", "country_name", "year", "ccodealp", "cow_code",
                           "code_wb", "dd_cga", "ht_region")

##############################
############ MEPV ############
##############################

# Read data
mepv <- read_excel("MEPV/MEPV2012ex.xls", col_names = TRUE, sheet = 1)

# MEPV is our source for political violence variable. 

# Change to lower case the variables names
names(mepv) <- tolower(names(mepv))

# Keep only the variables of interest 
varKeep <- c("year", "ccode", "country", "scode", "civviol", "civtot", "ethviol") 

# Remove variables
small_mepv <- mepv[,varKeep]

# Rename Variables
names(small_mepv) <- c("year", "ccode", "country_name", "country_abb", 
                       "civviol", "civtot", "ethviol")

##############################
############ IEP #############
##############################

# Read data
iep <- read.csv("IEP/IAEPv2_0_2015numeric.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# IEP is our second source for Party Bans. It also contains information if the country is federative

# Keep only the variables of interest 
varKeep <- c("cname", "cabr", "ccode", "year", 
             "banethnic", "banrelig", "bansys", "banall", 
             "govstruct", "lelecsystem", "banned", "elecleg", "elecexec") 

# Remove variables
small_iep <- iep[,varKeep]

# Rename variables
names(small_iep) <- c("country_name", "country_abb", "cow_code", "year", 
                      "banethnic", "banrelig", "bansys", "banall", "govstruct", 
                      "elecSystem", "partybanIEP", "elecleg", "elecexec")

# Recode variables

# Party bans
small_iep$partybanIEP <- as.numeric(small_iep$partybanIEP)

# Unitary
small_iep$unitary <- as.numeric(recode(small_iep$govstruct, "1 = 1;
                                       2 = 0;
                                       3 = 0"))
# Proportional System
small_iep$PRsystem <- as.numeric(recode(small_iep$elecSystem, "1=0; 2=0; 3=1; 4=1"))

###############################################
########## Fearon and Laitin Data  ############
###############################################

# Read data
ethnicdata <- read.dta("Fearon Data/repdata.dta")

# Fearon and Laitin Data is our dataset for ethnic fragmentation

# Keep only the variables of interest 
varKeep <- c("ccode", "country", "cname", "year", "ethfrac") 

# Remove variables
small_ethnicdata <- ethnicdata[,varKeep]

#####################################################
########## CREG Ethnic Fractionalization ############
#####################################################

# Read data
#EthnicRaw <- read.csv("EthnicGroupsWide_v1.02.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
EthnicRaw <- read.csv("AnaEthnicGroupsWide_v1.02.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Calculate the Ethnic Fractionalization Index
#EthnicRaw[,5:427][is.na(EthnicRaw[,5:427])] <- 0
#EthnicRaw[,5:427] <- EthnicRaw[,5:427]/100
#EthnicRaw$EthFrac <- apply(EthnicRaw[,5:427], 1, function(x) 1-sum(x^2))

# Keep only the Ethnic Fractionalization Index
#EthnicFraction <- EthnicRaw[, -(5:427)]

# Export to csv
#write.csv(EthnicFraction, file = "EthnicFraction.csv")

###################################################
############ Global Terrorism Database ############
###################################################

# Read data
gtd <- read_excel("GlobalTerrorismDataset/globalterrorismdb_0616dist.xlsx", col_names = TRUE,
                  sheet = 1)

# GTD contains data on terrorist attacks. However, the dataset for 1993 does not contain
# information if the attack was done by a domestic or a international group.

# Keep only the variables of interest 
varKeep <- c("eventid", "iyear", "country", "country_txt", "INT_LOG") 

# Remove variables
small_gtd <- gtd[,varKeep]

# Keep only domestic attacks (This variable does not exist for 1993):
small_gtd <- small_gtd[small_gtd[,"INT_LOG"]==0,]

# Generate the cumulative sum of terrorist attacks per country year:
small_gtd[, "attack"] <- 1
small_gtd <- ddply(.data = small_gtd, .variables = c("iyear", "country", "country_txt"), 
                   .fun = summarize, n_attack = sum(attack))

# Recode variable names
names(small_gtd) <- c("year", "ccodeGTD", "country", "n_attacks")

# Add codes to terrorism data:
source("Analysis Files/AddCodes.R")

####################################
############ Riots Data ############
###################################

# Read data
Africa <- read.csv("Riots/SCAD_Africa_32_Update.csv", stringsAsFactors = FALSE, header = TRUE)
LatAm <- read.csv("Riots/SCAD_LA_32.csv", stringsAsFactors = FALSE, header = TRUE)

# Keep variables
varKeep <- c("ccode", "styr", "etype")

# Remove variables
Africa_small <- Africa[,varKeep]
LatAm_small <- LatAm[,varKeep]

# Merge data
small_riots <- rbind.fill(Africa_small, LatAm_small)

# Select only riots against government
small_riots <- small_riots[small_riots[,"etype"]==8,]

# Colapse by year
small_riots[, "riot"] <- 1
small_riots <- ddply(.data = small_riots, .variables = c("styr", "ccode"), 
                     .fun = summarize, n_riots = sum(riot))

###################################
########## Merge data  ############
###################################

dataFinal <- data.frame(cow_code = NA, year = NA)
j <- 1
for(t in 1970:2015){
  for(i in unique(small_gtd$cow_code)){
    dataFinal[j ,"cow_code"] <- i 
    dataFinal[j ,"year"] <- t
    j <- j + 1
  }
}
dataFinal <- dataFinal[dataFinal[, "year"]!=1993,]
# Remove if cow_code is NA
dataFinal <- dataFinal[!is.na(dataFinal$cow_code),] 

# Merge with GDT
dataFinal <- merge(x = dataFinal, 
                   y = small_gtd, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

# Merge with V-Dem
dataFinal <- merge(x = dataFinal, 
                   y = smalldata_vdem, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

# Merge with QoG
dataFinal <- merge(x = dataFinal, 
                   y = smalldata_qgov, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)


# Merge Riots
#dataFinal <- merge(x = dataFinal, 
#                   y = small_riots, 
#                   by.x = c("year", "cow_code"), 
#                   by.y = c("styr", "ccode"),
#                   all.x = TRUE)

# Merge MEPV
dataFinal <- merge(x = dataFinal, 
                   y = small_mepv, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "ccode"),
                   all.x = TRUE)


# Merge with IEP
dataFinal <- merge(x = dataFinal, 
                   y = small_iep, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)


# Merge Latin and Fearon
#dataFinal <- merge(x = dataFinal, 
#                   y = small_ethnicdata, 
#                   by.x = c("year", "cow_code"), 
#                   by.y = c("year", "ccode"),
#                   all.x = TRUE)


# Merge Ethnic Fractionalization with other data
#dataFinal <- read.csv("dataFinal.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#dataFinal <- merge(x = dataFinal, 
#y = EthnicFraction, 
#by.x = c("year", "cow_code"), 
#by.y = c("Year", "Cowcode"),
#all.x = TRUE)
EthnicFraction <- read.csv("Analysis Files/EthnicFraction.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
dataFinal <- merge(x = dataFinal, 
                   y = EthnicFraction, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("Year", "Cowcode"),
                   all.x = TRUE)


##########################################################
########## Generate Variables and clean data  ############
##########################################################

# We still need to recover the countries names and add them to only one variable
# Remove country variables names
RemVar <- c("country_id", "country_abb.x", "country_abb.y", "country_name.x",
            "country", "cname", "country_name.y", "country_abb", "country_name.y.1",
            "country_name", "country_name.x.1", "country_name.y", "X", "Country")
for(i in RemVar){
  dataFinal[, i] <- NULL
}

# Rename country_name variable
names(dataFinal)[13] <- "country_name"

# The variable n_attacks does not contains 0. 
# We have to add them.
dataFinal$n_attacks[is.na(dataFinal$n_attacks)] <- 0

# Order the dataset
dataFinal <- dataFinal[order(dataFinal$cow_code, dataFinal$year),]

# Generate First Difference n_attacks
dataFinal <- dataFinal %>%
  group_by(cow_code) %>%
  mutate(fd_n_attacks = n_attacks - lag(n_attacks))

# Generate Dummy with civtot
dataFinal$d_civtot <- recode(dataFinal$civtot, "0=0; else=1")

# Remove countries that don't have elections
dataFinal <- subset(dataFinal, subset = elecleg==1)

# Rename new EthFrac variable
names(dataFinal)[29] <- "NewEthFrac"

# Generate unified partybans
dataFinal$partybanUni <- ifelse((dataFinal$partybanIEP==1 & dataFinal$party_banVD==1), 1, 
                                ifelse((dataFinal$partybanIEP==0 & dataFinal$party_banVD==0), 0,
                                       ifelse((dataFinal$partybanIEP==1 & dataFinal$party_banVD==0),
                                              0, ifelse((dataFinal$partybanIEP==0 & dataFinal$party_banVD==1),
                                                        0, NA))))


#################################
########## Save data  ###########
#################################

# Export to csv
write.csv(dataFinal, file = "Analysis Files/dataFinal.csv")
write.dta(dataFinal, file = "Analysis Files/dataFinal.dta")

# Luwei Export
#write.csv(dataFinal, file = "dataFinal.csv")


#######################################
########## data Discription ###########
#######################################
dataFinal <- read.csv("dataFinal.csv", header=T)
length(unique(dataFinal$cow_code)) #133
length(unique(dataFinal$ccodealp)) #132 I don't know how these two different. I count the former as the number of countries.

# Find any missing values
apply(dataFinal, 2, function(x) sum(is.na(x)))

# summary some critical variables
stargazer(dataFinal)

# Find the distribution of the countries
continent <- table(countrycode(sourcevar=unique(dataFinal$cow_code), origin="cown", destination="continent", warn=T))
# Use the Cowcode table to classify the unmatached cases: 260, 265, 345, 713. After that, plus 3 to Europe and 1 to Asia.
COWcode <- read.csv("dataFinal.csv", header=T)

# Institutions
nrow(dataFinal[dataFinal$dd_cga==1,])
nrow(dataFinal[dataFinal$dd_cga==0,])
nrow(dataFinal[dataFinal$govstruct==1,])
nrow(dataFinal[dataFinal$govstruct==2,])
nrow(dataFinal[dataFinal$govstruct==3,])
nrow(dataFinal[dataFinal$elecSystem==1,])
nrow(dataFinal[dataFinal$elecSystem==2,])
nrow(dataFinal[dataFinal$elecSystem==3,])
nrow(dataFinal[dataFinal$elecSystem==4,])

# An overview of party bans
allKindsBans <- dataFinal[,c(5, 18, 19, 20, 21, 24)]
sapply(allKindsBans, function(x) sum(!is.na(x)))
colSums(allKindsBans) # Observations
c(115, 181, 1172, 436)/4349 
sum(na.omit(dataFinal$partybanIEP))
1782/4340 # Proportion by IEP
sum(na.omit(dataFinal$party_banVD))
1495/4272 # Proportion by V-Dem
sum(!is.na(dataFinal$dd_cga))

# Differetiate between democracy and dictatorship
Democ <- subset(dataFinal,dataFinal$dd_cga==1)
Dictat <- subset(dataFinal,dataFinal$dd_cga==0)
sum(na.omit(Democ[,24]))
sum(na.omit(Dictat[,24]))
table(dataFinal$dd_cga)


###############################
########## Analysis ###########
###############################


# Keep only complete cases
dataFinalComp <- dataFinal[complete.cases(dataFinal),]

# OLS with attacks
summary(plm(n_attacks ~ banall + polity2 + factor(lelecsystem) + 
              log(oil_gas_valuePOP_2000 + 1) + log(pop_maddison + 1), 
            data = dataFinal, index=c("scode", "year"),  model="within"))
# Ethnic has unit root.

# OLS with attacks
summary(plm(civtot ~ banall + polity2 + factor(lelecsystem) + 
              log(oil_gas_valuePOP_2000 + 1) + log(pop_maddison + 1), 
            data = dataFinal, index=c("scode", "year"),  model="within"))

sum(complete.cases(dataFinal))
for(i in 1:length(dataFinal)){
  print(names(dataFinal)[i])
  print(table(is.na(dataFinal[,i])))
}

sum(complete.cases(dataFinal))
nrow(dataFinal)
