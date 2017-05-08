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
for(t in 1990:2015){
   for(i in unique(small_riots$ccode)){
      dataFinal[j ,"cow_code"] <- i 
      dataFinal[j ,"year"] <- t
   j <- j + 1
   }
}

# Remove if cow_code is NA
dataFinal <- dataFinal[!is.na(dataFinal$cow_code),] 

# Merge with Riots
dataFinal <- merge(x = dataFinal, 
                   y = small_riots, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("styr", "ccode"),
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

# Merge with IEP
dataFinal <- merge(x = dataFinal, 
                   y = small_iep, 
                   by.x = c("year", "cow_code"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)


# Merge Ethnic Fractionalization with other data
#dataFinal <- read.csv("dataFinal.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#dataFinal <- merge(x = dataFinal, 
                   #y = EthnicFraction, 
                   #by.x = c("year", "cow_code"), 
                   #by.y = c("Year", "Cowcode"),
                   #all.x = TRUE)
EthnicFraction <- read.csv("Analysis Files/EthnicFraction.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
EthnicFraction <- EthnicFraction[, c("Cowcode", "Year", "EthFrac")]
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
            "country", "cname", "country_name.y", "country_abb", "country_name.y.1",
            "country_name", "country_name.x.1", "country_name.y", "X", "Country")
for(i in RemVar){
   dataFinal[, i] <- NULL
}

# Rename country_name variable
names(dataFinal)[4] <- "country_name"

# The variable n_riots does not contains 0. 
# We have to add them.
dataFinal$n_riots[is.na(dataFinal$n_riots)] <- 0

# Order the dataset
dataFinal <- dataFinal[order(dataFinal$cow_code, dataFinal$year),]

# Remove countries that don't have elections
dataFinal <- subset(dataFinal, subset = elecleg==1)

# Rename new EthFrac variable
names(dataFinal)[25] <- "NewEthFrac"

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
write.csv(dataFinal, file = "Analysis Files/dataFinalRiots.csv")
write.dta(dataFinal, file = "Analysis Files/dataFinalRiots.dta")

# Luwei Export
#write.csv(dataFinal, file = "dataFinal.csv")

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

