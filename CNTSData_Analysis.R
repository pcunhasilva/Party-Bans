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
library(readxl)
library(countrycode)

# Set Directory:
wd <- "~/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5262 - Comparative Party Politics/Data/"
setwd(wd)

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
EthnicFraction <- read.csv("Analysis Files/EthnicFraction.csv", sep = ",", 
                           header = TRUE, stringsAsFactors = FALSE)
EthnicFraction <- EthnicFraction[, c("Cowcode", "Year", "EthFrac")]

#################################
########## CNTS Data ############
#################################

CNTSData <- read_excel("CNTS Data/2016 Edition CNTSDATA.xlsx")

# Keep only the variables of interest 
varKeep <- c("World Bank Code", "Year", "Riots", "Anti-Government Demonstrations") 
smallCNTSData <- CNTSData[,varKeep]

# Rename Variables
names(smallCNTSData) <- c("wbcode", "year", "riots", "antigov")

# Recode Variables into numeric
smallCNTSData$year <- as.numeric(smallCNTSData$year)
smallCNTSData$riots <- as.numeric(smallCNTSData$riots)
smallCNTSData$antigov <- as.numeric(smallCNTSData$antigov)

# Keep data only after 1959
smallCNTSData <- subset(smallCNTSData, year>1959)

# Add COW code
smallCNTSData$cow <- countrycode(smallCNTSData$wbcode, "wb", "cown")

###################################
########## Merge data  ############
###################################

# Merge with V-Dem
dataFinal <- merge(x = smallCNTSData, 
                   y = smalldata_vdem, 
                   by.x = c("year", "cow"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)
# Merge with QoG
dataFinal <- merge(x = dataFinal, 
                   y = smalldata_qgov, 
                   by.x = c("year", "cow"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

# Merge with IEP
dataFinal <- merge(x = dataFinal, 
                   y = small_iep, 
                   by.x = c("year", "cow"), 
                   by.y = c("year", "cow_code"),
                   all.x = TRUE)

# Merge with Ethnic Fractionalization
dataFinal <- merge(x = dataFinal, 
                   y = EthnicFraction, 
                   by.x = c("year", "cow"), 
                   by.y = c("Year", "Cowcode"),
                   all.x = TRUE)


# Remove countries that don't have elections
dataFinal <- subset(dataFinal, subset = elecleg==1)

# Rename new EthFrac variable
names(dataFinal)[32] <- "NewEthFrac"

# Generate unified partybans
dataFinal$partybanUni <- ifelse((dataFinal$partybanIEP==1 & dataFinal$party_banVD==1), 1, 
                                ifelse((dataFinal$partybanIEP==0 & dataFinal$party_banVD==0), 0,
                                       ifelse((dataFinal$partybanIEP==1 & dataFinal$party_banVD==0),
                                              0, ifelse((dataFinal$partybanIEP==0 & dataFinal$party_banVD==1),
                                                        0, NA))))

#################################################
############### Run the models ##################
#################################################

## Generate oil measure
dataFinal$oilpop <- log(((dataFinal$oilHM * 1000000000)/dataFinal$popHM)+1)

## Generate Lag riots
dataFinal$lag_riots <- lag(dataFinal$riots)

## Generate Lag antigov
dataFinal$lag_antigov <- lag(dataFinal$antigov)

## Run full model with riots as DV, 
## the interaction of party ban and polity and controls with fixed effects
model <- glm(riots ~ lag_riots + partybanUni*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow), data = dataFinal,
             family="poisson") 
summary(model)

## Call coefficients and their std errors from model and sample 1000 times
simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

## Set-up empty results matrix
results <- matrix(NA, nrow = 20, ncol = 1000)

## Create matrix of betas for each value of polity (from -9 to 10) where party ban=1
## Hold continuous controls at means, hold unitary and PRsystem at 1
## Hold country dummies at zero
values <- matrix(c(1, mean(model.frame(model)$lag_riots),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 138), -9), nrow = 1)

## Multiply each value of polity by the coefficients in the simulation to get y*
j <- 1
for (i in -9:10){
   values[,c(4, 147)] <- i
   results[j,] <- values %*% t(simulation)
   j <- j+1
}

## Get mean number of attacks for each value of polity
meanY <- rowMeans(exp(results))

## Get 95% confidence interval for each value of polity
CI <- matrix(NA, ncol = 2, nrow = 20)
for(i in 1:20){
   orderMatrix <- order(exp(results)[1,])
   CI[i, 1] <- exp(results[i,])[orderMatrix][25]
   CI[i, 2] <- exp(results[i,])[orderMatrix][975]
}

## Repeat process for when party ban=0
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1, mean(model.frame(model)$lag_riots),
                    1, 0, 1, mean(model.frame(model)$NewEthFrac), 1,
                    mean(model.frame(model)$oilpop), rep(0, 138), 0), nrow = 1)
j <- 1
for (i in -9:10){
   values2[,c(4)] <- i
   results2[j,] <- values2 %*% t(simulation)
   j <- j+1
}

CI2 <- matrix(NA, ncol = 2, nrow = 20)
meanY2 <- rowMeans(exp(results2))
for(i in 1:20){
   orderMatrix <- order(exp(results2)[1,])
   CI2[i, 1] <- exp(results2[i,])[orderMatrix][25]
   CI2[i, 2] <- exp(results2[i,])[orderMatrix][975]
}


## Plot means and confidence intervals for party ban=0 and party ban=1
plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2)-20, max(CI, CI2))+10, col = "red",
     xlim = c(-9, 10), xaxt='n', xlab='Polity', 
     ylab='Number of Riots', main='Number of Riots by Polity and Party Ban')
axis(side=1,at=seq(-9,10,by=1))

j <- 1
for(i in -9:10){
   lines(y = c(CI[j, 1], CI[j, 2]), x = c(i, i), col = "red")
   j <-  j + 1
}

points(x = -9:10, y= meanY2, pch = 18, col = "blue")
j <- 1
for(i in -9:10){
   lines(y = c(CI2[j, 1], CI2[j, 2]), x = c(i, i), col = "blue")
   j <-  j + 1
}

legend('topright',pch=c(20,18),col=c('red','blue'),
       legend=c('Party Ban', 'No Party Ban'), bty='n')

# Repeat everything with anti-government demonstrations
## Run full model with riots as DV, 
## the interaction of party ban and polity and controls with fixed effects
model <- glm(antigov ~ lag_antigov + partybanUni*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow), data = dataFinal,
             family="poisson") 
summary(model)

## Call coefficients and their std errors from model and sample 1000 times
simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

## Set-up empty results matrix
results <- matrix(NA, nrow = 20, ncol = 1000)

## Create matrix of betas for each value of polity (from -9 to 10) where party ban=1
## Hold continuous controls at means, hold unitary and PRsystem at 1
## Hold country dummies at zero
values <- matrix(c(1, mean(model.frame(model)$lag_antigov),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 138), -9), nrow = 1)

## Multiply each value of polity by the coefficients in the simulation to get y*
j <- 1
for (i in -9:10){
   values[,c(4, 147)] <- i
   results[j,] <- values %*% t(simulation)
   j <- j+1
}


## Get mean number of attacks for each value of polity
meanY <- rowMeans(exp(results))

## Get 95% confidence interval for each value of polity
CI <- matrix(NA, ncol = 2, nrow = 20)
for(i in 1:20){
   orderMatrix <- order(exp(results)[1,])
   CI[i, 1] <- exp(results[i,])[orderMatrix][25]
   CI[i, 2] <- exp(results[i,])[orderMatrix][975]
}

## Repeat process for when party ban=0
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1, mean(model.frame(model)$lag_antigov),
                    1, 0, 1, mean(model.frame(model)$NewEthFrac), 1,
                    mean(model.frame(model)$oilpop), rep(0, 138), 0), nrow = 1)
j <- 1
for (i in -9:10){
   values2[,c(4)] <- i
   results2[j,] <- values2 %*% t(simulation)
   j <- j+1
}

CI2 <- matrix(NA, ncol = 2, nrow = 20)
meanY2 <- rowMeans(exp(results2))
for(i in 1:20){
   orderMatrix <- order(exp(results2)[1,])
   CI2[i, 1] <- exp(results2[i,])[orderMatrix][25]
   CI2[i, 2] <- exp(results2[i,])[orderMatrix][975]
}


## Plot means and confidence intervals for party ban=0 and party ban=1
plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2)-20, max(CI, CI2))+10, col = "red",
     xlim = c(-9, 10), xaxt='n', xlab='Polity', 
     ylab='Number of \n Anti-Government Demonstrations', 
     main='Number of Anti-Government Demonstration \n by Polity and Party Ban')
axis(side=1,at=seq(-9,10,by=1))

j <- 1
for(i in -9:10){
   lines(y = c(CI[j, 1], CI[j, 2]), x = c(i, i), col = "red")
   j <-  j + 1
}

points(x = -9:10, y= meanY2, pch = 18, col = "blue")
j <- 1
for(i in -9:10){
   lines(y = c(CI2[j, 1], CI2[j, 2]), x = c(i, i), col = "blue")
   j <-  j + 1
}

legend('topright',pch=c(20,18),col=c('red','blue'),
       legend=c('Party Ban', 'No Party Ban'), bty='n')
