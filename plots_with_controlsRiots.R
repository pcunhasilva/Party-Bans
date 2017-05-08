rm(list = ls())

library(mvtnorm)

riots <- read.csv("Analysis Files/dataFinalRiots.csv", stringsAsFactors = FALSE, header = TRUE)

## Generate oil measure
riots$oilpop <- log(((riots$oilHM * 1000000000)/riots$popHM)+1)

## Generate Lag riots
riots$lag_nriots <- lag(riots$n_riots)

attach(riots)
model <- glm(n_riots ~ lag_nriots + partybanUni*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = riots,
             family="poisson") 
summary(model)


simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

results <- matrix(NA, nrow = 20, ncol = 1000)
values <- matrix(c(1, mean(model.frame(model)$lag_nriots),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 47), -9), nrow = 1)
j <- 1
for (i in -9:10){
   values[,c(4, 56)] <- i
   results[j,] <- values %*% t(simulation)
   j <- j+1
}

CI <- matrix(NA, ncol = 2, nrow = 20)
meanY <- rowMeans(exp(results))
for(i in 1:20){
   orderMatrix <- order(exp(results)[1,])
   CI[i, 1] <- exp(results[i,])[orderMatrix][50]
   CI[i, 2] <- exp(results[i,])[orderMatrix][950]
}



##################
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1,mean(model.frame(model)$lag_nriots), 
                    0, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 47), 0), nrow = 1)
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
   CI2[i, 1] <- exp(results2[i,])[orderMatrix][50]
   CI2[i, 2] <- exp(results2[i,])[orderMatrix][950]
}

plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2), max(CI, CI2)), col = "red")
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

#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

model <- glm(n_riots ~ lag_nriots + partybanIEP*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = riots,
             family="poisson") 
summary(model)


simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

results <- matrix(NA, nrow = 20, ncol = 1000)
values <- matrix(c(1, mean(model.frame(model)$lag_nriots),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 47), -9), nrow = 1)
j <- 1
for (i in -9:10){
   values[,c(4, 56)] <- i
   results[j,] <- values %*% t(simulation)
   j <- j+1
}

CI <- matrix(NA, ncol = 2, nrow = 20)
meanY <- rowMeans(exp(results))
for(i in 1:20){
   orderMatrix <- order(exp(results)[1,])
   CI[i, 1] <- exp(results[i,])[orderMatrix][50]
   CI[i, 2] <- exp(results[i,])[orderMatrix][950]
}



##################
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1,mean(model.frame(model)$lag_nriots), 
                    0, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                    mean(model.frame(model)$oilpop), rep(0, 47), 0), nrow = 1)
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
   CI2[i, 1] <- exp(results2[i,])[orderMatrix][50]
   CI2[i, 2] <- exp(results2[i,])[orderMatrix][950]
}

plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2), max(CI, CI2)), col = "red")
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


#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

model <- glm(n_riots ~ lag_nriots + party_banVD*polity2 + PRsystem +
                NewEthFrac + unitary + oilpop +
                as.factor(cow_code), data = riots,
             family="poisson") 
summary(model)


simulation <- rmvnorm(1000, mean = coef(model), sigma = vcov(model))

results <- matrix(NA, nrow = 20, ncol = 1000)
values <- matrix(c(1, mean(model.frame(model)$lag_nriots),
                   1, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                   mean(model.frame(model)$oilpop), rep(0, 47), -9), nrow = 1)
j <- 1
for (i in -9:10){
   values[,c(4, 56)] <- i
   results[j,] <- values %*% t(simulation)
   j <- j+1
}

CI <- matrix(NA, ncol = 2, nrow = 20)
meanY <- rowMeans(exp(results))
for(i in 1:20){
   orderMatrix <- order(exp(results)[1,])
   CI[i, 1] <- exp(results[i,])[orderMatrix][50]
   CI[i, 2] <- exp(results[i,])[orderMatrix][950]
}



##################
results2 <- matrix(NA, nrow = 20, ncol = 1000)
values2 <- matrix(c(1,mean(model.frame(model)$lag_nriots), 
                    0, -9, 1, mean(model.frame(model)$NewEthFrac), 1,
                    mean(model.frame(model)$oilpop), rep(0, 47), 0), nrow = 1)
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
   CI2[i, 1] <- exp(results2[i,])[orderMatrix][50]
   CI2[i, 2] <- exp(results2[i,])[orderMatrix][950]
}

plot(x = -9:10, y = meanY, pch = 20,
     ylim= c(min(CI, CI2), max(CI, CI2)), col = "red")
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

