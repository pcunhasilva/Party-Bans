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
library(pglm)

# Set Directory:
wd <- "/Users/patrickcunhasilva/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/"
setwd(wd)

###################################################
############ Global Terrorism Database ############
###################################################

# Read data
gtd <- read_excel("GlobalTerrorismDataset/globalterrorismdb_0616dist.xlsx", col_names = TRUE,
                  sheet = 1)
gtd1993 <- read_excel("GlobalTerrorismDataset/gtd1993_0616dist.xlsx", col_names = TRUE,
                    sheet = 1)

# Observe the variables in the data
str(gtd)

# Keep only the variables of interest 
varKeep <- c("eventid", "iyear", "country", "country_txt") 

# Remove variables
gtd <- gtd[,varKeep]
gtd1993 <- gtd1993[, varKeep]

# Generate the cumulative sum of terrorist attacks per country year:
gtd[, "attack"] <- 1
gtd1993[, "attack"] <- 1
tpc <- ddply(.data = gtd, .variables = c("iyear", "country", "country_txt"), 
      .fun = summarize, n_attack = sum(attack))
tpc1993 <- ddply(.data = gtd1993, .variables = c("iyear", "country", "country_txt"), 
             .fun = summarize, n_attack = sum(attack))

# Merge 1993 with the rest of the data:
tpcFinal <- rbind(tpc, tpc1993)

# Recode variable names
names(tpcFinal) <- c("year", "ccodeGTD", "country", "n_attacks")

# Remove all auxiliary files
rm(varKeep, gtd, gtd1993, tpc, tpc1993)

###################################
############ Polity IV ############
###################################

# Read data
polityIV <- read_excel("PolityIV/p4v2015.xls", col_names = TRUE, sheet = 1)

# Observe the variables in the data
str(polityIV)

# Keep only the variables of interest 
varKeep <- c("year", "ccode", "country", "scode", "polity2") 

# Remove variables
polityIV <- polityIV[,varKeep]

# Remove observations with year<1946
polityIVFinal <- subset(polityIV, subset = year>1945)

# Remove all auxiliary files
rm(varKeep, polityIV)

##############################
############ MEPV ############
##############################

# Read data
mepv <- read_excel("MEPV/MEPV2012ex.xls", col_names = TRUE, sheet = 1)

# Observe the variables in the data
str(mepv)

# Change to lowe case the variables names
names(mepv) <- tolower(names(mepv))

# Keep only the variables of interest 
varKeep <- c("year", "ccode", "country", "scode", "civviol", "civtot", "ethviol") 

# Remove variables
mepvFinal <- mepv[,varKeep]

# Remove all auxiliary files
rm(varKeep, mepv)

##############################
############ IEP #############
##############################

# Read data
iep <- read.dta13("IEP/IAEPv2_0_2015.dta")

# Observe the variables in the data
str(iep)

# Keep only the variables of interest 
varKeep <- c("cname", "cabr", "ccode", "year", 
             "banethnic", "banrelig", "bansys", "banall", "lelecsystem") 

# Remove variables
iepFinal <- iep[,varKeep]

# Remove all auxiliary files
rm(varKeep, iep)

############################################
############ Ross and Mahdavi  #############
############################################

# Read data
oildata <- read.csv("Ross/Ross-Mahdavi Oil and Gas 1932-2014.csv", header = TRUE,
                    stringsAsFactors = FALSE, sep = ",")

# Observe the variables in the data
str(oildata)

# Keep only the variables of interest 
varKeep <- c("cty_name", "iso3numeric", "id", "year", 
             "eiacty", "oil_gas_valuePOP_2000", "pop_maddison") 

# Remove variables
oildataFinal <- oildata[,varKeep]

# Remove all auxiliary files
rm(varKeep, oildata)

###############################################
########## Fearon and Laitin Data  ############
###############################################

# Read data
ethnicdata <- read.dta("Fearon Data/repdata.dta")

# Observe the variables in the data
str(ethnicdata)

# Keep only the variables of interest 
varKeep <- c("ccode", "country", "cname", "year", "ethfrac") 

# Remove variables
ethnicdataFinal <- ethnicdata[,varKeep]

# Remove all auxiliary files
rm(varKeep, ethnicdata)

###################################
########## Merge data  ############
###################################

# Add codes to terrorism data:
source("Analysis Files/AddCodes.R")

# Merge polityIV data with mepv
dataFinal <- merge(x = polityIVFinal, 
                   y = mepvFinal, 
                   by.x = c("year", "scode"), 
                   by.y = c("year", "scode"),
                   all = TRUE)

# Merge oilData data:
dataFinal <- merge(x = dataFinal, 
                   y = oildataFinal, 
                   by.x = c("year", "scode"), 
                   by.y = c("year", "id"),
                   all = TRUE)

# Merge tpcFinal data
dataFinal <- merge(x = dataFinal, 
                   y = tpcFinal, 
                   by.x = c("year", "scode"), 
                   by.y = c("year", "acode"),
                   all = TRUE)

# Merge ethnic data
dataFinal <- merge(x = dataFinal, 
                   y = ethnicdataFinal, 
                   by.x = c("year", "ccode.x"), 
                   by.y = c("year", "ccode"),
                   all = TRUE)

# Merge party ban data
dataFinal <- merge(x = dataFinal, 
                   y = iepFinal, 
                   by.x = c("year", "ccode.x"), 
                   by.y = c("year", "ccode"),
                   all.y = TRUE)

###########################################
########## Clean and save data  ###########
###########################################

# Keep only complete cases
#dataFinal <- dataFinal[complete.cases(dataFinal),]

# Remove observations before 1960
dataFinal <- subset(dataFinal, subset = year>1959)

# Export to csv
write.csv(dataFinal, file = "Analysis Files/dataFinal.csv")




#####
#pglm(pat ~ lag(logr, 0:5) + scisect + logk + factor(year), PatsRD,
 #    family = negbin, model = "within", print.level=3, method="nr",
  #   index=c('cusip', 'year'))