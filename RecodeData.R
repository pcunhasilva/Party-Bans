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

# Set Directory:
wd <- "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/"
setwd(wd)
setwd("/Users/luweiying/Desktop/Comparative Party Politics/Party_Bans/DataAnalysis")

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
varKeep <- c("ccode", "cname", "year", "ccodealp","ccodecow", "ccodewb") 

# Remove variables and save the final version
smalldata_qgov <- data_qgov[,varKeep]

# Rename variables
names(smalldata_qgov) <- c("ccode", "country_name", "year", "ccodealp", "cow_code",
                           "code_wb")

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
# Unitary
small_iep$unitary <- recode(small_iep$govstruct, "1 = 1;
                                2:3 = 0")
# Proportional System
small_iep$PRsystem <- recode(small_iep$elecSystem, "1=0; 2=0; else=1")

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
EthnicRaw <- read.csv("EthnicGroupsWide_v1.02.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Calculate the Ethnic Fractionalization Index
EthnicRaw[,5:427][is.na(EthnicRaw[,5:427])] <- 0
EthnicRaw[,5:427] <- EthnicRaw[,5:427]/100
EthnicRaw$EthFrac <- apply(EthnicRaw[,5:427], 1, function(x) 1-sum(x^2))

# Keep only the Ethnic Fractionalization Index
EthnicFraction <- EthnicRaw[, -(5:427)]

# Export to csv
write.csv(EthnicFraction, file = "EthnicFraction.csv")

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

###################################
########## Merge data  ############
###################################

# Merge V-Dem with QoG
dataFinal <- merge(x = smalldata_vdem, 
                   y = smalldata_qgov, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

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
dataFinal <- merge(x = dataFinal, 
                   y = small_ethnicdata, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "ccode"),
                   all.x = TRUE)


# Merge GDT data
dataFinal <- merge(x = dataFinal, 
                   y = small_gtd, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

# Merge Ethnic Fractionalization with other data
dataFinal <- read.csv("dataFinal.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
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
RemVar <- c("country_id", "country_abb.x", "country_abb.y",
            "country", "cname", "country_name.y", "country_abb", 
            "country_name", "country_name.x.1", "country_name.y")
for(i in RemVar){
   dataFinal[, i] <- NULL
}

# Remove if cow_code is NA
dataFinal <- dataFinal[!is.na(dataFinal$cow_code),] 

# Rename country_name variable
names(dataFinal)[3] <- "country_name"

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

# Generate log(oilcap)
dataFinal$lnoilcap <- log(dataFinal$oilHM/dataFinal$popHM + 1)

# Generate log(population)
dataFinal$lnpop <- log(dataFinal$popHM)

# Remove countries that don't have elections
dataFinal <- subset(dataFinal, subset = elecleg==1)

#################################
########## Save data  ###########
#################################

# Export to csv
write.csv(dataFinal, file = "Analysis Files/dataFinal.csv")

# Luwei Export
write.csv(dataFinal, file = "dataFinal.csv")

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

