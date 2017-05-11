##################################################################
### Interaction of Terrorist Attacks with Polity and Party Ban ###
### Patrick Cunha Silva, William O'Brochta, Luwei Ying         ###
##################################################################

library(mvctm)

## Read terrorist attacks dataset
attacks <- read.csv("dataFinal.csv", stringsAsFactors = FALSE, header = TRUE)

## Generate oil measure
attacks$oilpop <- log(((attacks$oilHM * 1000000000)/attacks$popHM)+1)

## Generate Lag attacks
attacks$lag_nattacks <- lag(attacks$n_attacks)

## Run full model with attacks as DV, 
## the interaction of party ban and polity and controls with fixed effects
model <- glm(n_attacks ~ lag_nattacks + partybanUni*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = attacks,
             family="poisson") 
summary(model)

## Call coefficients and their std errors from model and sample 1000 times
simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

## Set-up empty results matrix
results <- matrix(NA, nrow = 20, ncol = 1000)

## Create matrix of betas for each value of polity (from -9 to 10) where party ban=1
## Hold continuous controls at means, hold unitary and PRsystem at 1
## Hold country dummies at zero
values <- matrix(c(1, mean(model.frame(model)$lag_nattacks),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 114), -9), nrow = 1)

## Multiply each value of polity by the coefficients in the simulation to get y*
j <- 1
for (i in -9:10){
   values[,c(4, 123)] <- i
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


##################
## Repeat process for when party ban=0
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1, mean(model.frame(model)$lag_nattacks),
                   1, 0, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 114), 0), nrow = 1)
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
     ylab='Number of Attacks', main='Number of Attacks by Polity and Party Ban')
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



## Robustness Checks on party ban measure             ##
########################################################
########################################################
########################################################
########################################################
## Consider only the IEP measure of party ban.

model2 <- glm(n_attacks ~ lag_nattacks + partybanIEP*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = attacks,
             family="poisson") 
summary(model2)


simulation <- rmvnorm(1000, mean = coef(model2), sigma = vcov(model2))

results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1, mean(model.frame(model2)$lag_nattacks),
                   1, -9, 1, mean(model.frame(model2)$NewEthFrac), 1,
                   mean(model.frame(model2)$oilpop), rep(0, 114), -9), nrow = 1)

j <- 1
for (i in -9:10){
   values2[,c(4, 123)] <- i
   results2[j,] <- values2 %*% t(simulation)
   j <- j+1
}


CI <- matrix(NA, ncol = 2, nrow = 20)
meanY <- rowMeans(exp(results2))
for(i in 1:20){
   orderMatrix <- order(exp(results2)[1,])
   CI[i, 1] <- exp(results2[i,])[orderMatrix][25]
   CI[i, 2] <- exp(results2[i,])[orderMatrix][975]
}


##################
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1, mean(model.frame(model2)$lag_nattacks),
                    1, 0, 1, mean(model.frame(model2)$NewEthFrac), 1,
                    mean(model.frame(model2)$oilpop), rep(0, 114), 0), nrow = 1)
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

plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2)-20, max(CI, CI2))+10, col = "red",
     xlim = c(-9, 10), xaxt='n', xlab='Polity', 
     ylab='Number of Attacks', main='Number of Attacks by Polity and Party Ban')
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


########################################################
########################################################
########################################################
########################################################
## Consider only the VDemocracy measure of party ban.

model3 <- glm(n_attacks ~ lag_nattacks + party_banVD*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = attacks,
             family="poisson") 
summary(model3)


simulation <- rmvnorm(1000, mean = coef(model3), sigma = vcov(model))

results3 <- matrix(NA, nrow = 20, ncol = 1000)
values3 <- matrix(c(1, mean(model.frame(model3)$lag_nattacks),
                   1, -9, 1, mean(model.frame(model3)$NewEthFrac), 1,
                   mean(model.frame(model3)$oilpop), rep(0, 114), -9), nrow = 1)

j <- 1
for (i in -9:10){
   values3[,c(4, 123)] <- i
   results3[j,] <- values3 %*% t(simulation)
   j <- j+1
}


CI <- matrix(NA, ncol = 2, nrow = 20)
meanY <- rowMeans(exp(results3))
for(i in 1:20){
   orderMatrix <- order(exp(results3)[1,])
   CI[i, 1] <- exp(results3[i,])[orderMatrix][25]
   CI[i, 2] <- exp(results3[i,])[orderMatrix][975]
}


##################
results3 <- matrix(NA, nrow = 20, ncol = 1000)
values3 <- matrix(c(1, mean(model.frame(model3)$lag_nattacks),
                    1, 0, 1, mean(model.frame(model3)$NewEthFrac), 1,
                    mean(model.frame(model3)$oilpop), rep(0, 114), 0), nrow = 1)
j <- 1
for (i in -9:10){
   values3[,c(4)] <- i
   results3[j,] <- values3 %*% t(simulation)
   j <- j+1
}

CI2 <- matrix(NA, ncol = 2, nrow = 20)
meanY2 <- rowMeans(exp(results3))
for(i in 1:20){
   orderMatrix <- order(exp(results3)[1,])
   CI2[i, 1] <- exp(results3[i,])[orderMatrix][25]
   CI2[i, 2] <- exp(results3[i,])[orderMatrix][975]
}

plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2)-20, max(CI, CI2))+10, col = "red",
     xlim = c(-9, 10), xaxt='n', xlab='Polity', 
     ylab='Number of Attacks', main='Number of Attacks by Polity and Party Ban')
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