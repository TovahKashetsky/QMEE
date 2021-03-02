# ASSINGMENT 3 #
library(tidyverse)
ant_test <- read_csv("ant_testdata.csv")
print(ant_test)
# two columns named colony, change colony_1 to video
library(dplyr)
ant_test <- rename(ant_test, video = colony_1)
print(ant_test)
# make replicate and video a factor
ant_test <- (ant_test %>% mutate(replicate=as.factor(replicate)))
ant_test <- (ant_test %>% mutate(video=as.factor(video)))
summary(ant_test)
# remove empty rows because janiotr package didn't work
ant_test <- ant_test[-(21:26), ]
# make the ggplots assignment 3
theme_set(theme_bw())
# white background, make see through with alpha
plotdecision <- ggplot(ant_test, aes(group, decision_lat, colour=group)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")
plotdark <- ggplot(ant_test, aes(group, prop_dark, colour=group))+geom_boxplot(alpha=0.3) + labs(y="Proportion of transports to the dark nest") + theme(legend.position="none")
plotcohesion <- ggplot(ant_test, aes(group, cohesion, colour=group))+geom_boxplot(alpha=0.3) + labs(y="Cohesiveness") + theme(legend.position="none")
#make plots for grad research day
theme_set(theme_classic(base_size=35))
plotdecision1 <- ggplot(ant_test, aes(group, decision_lat)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")+ stat_sum(alpha=0.3)+ scale_size(range=c(4,5))
plotdark1 <- ggplot(ant_test, aes(group, prop_dark))+geom_boxplot(alpha=0.3) + labs(y="Proportion of transports to the dark nest") + theme(legend.position="none") +stat_sum(alpha=0.3)+ scale_size(range=c(5,6))
plotcohesion1 <- ggplot(ant_test, aes(group, cohesion))+geom_boxplot(alpha=0.3) + labs(y="Cohesiveness") + theme(legend.position="none")+stat_sum(alpha=0.3)+ scale_size(range=c(4,5))
print(plotdecision)
print(plotdark)
print(plotcohesion)
print(plotdecision1)
print(plotdark1)
print(plotcohesion1)


## Assignment 5 Permutations ##

# Hypothesis 2 from text file: choice group will perform better on a decision-making task than the no choice group
library(tidyverse)
library(dplyr)

# Test 1a: T-test - decision speed
nortt <- t.test(decision_lat~group, data=ant_test, var.equal=TRUE) #normal t test
obs <- nortt$statistic

res <- numeric(9999)
for (i in 1:9999) {
  perm <- sample(nrow(ant_test))
  pdat <-transform(ant_test,decision_lat=decision_lat[perm])
  tt <- t.test(decision_lat~group, data=pdat, car.equal=TRUE)
  res[i] <- tt$statistic
}
res[2] #checking for different values
res[3]

hist(res,col="gray",las=1,main="") 
abline(v=obs,col="red")
allvals <- c(res, obs)
mean(allvals>=obs)
(obs_pval <- 2*mean(allvals>=obs))
# it looks like the choice and no choice groups do not differ in decision latency

# Test 1b: T-test - efficiency 
nortt2 <- t.test(prop_dark~group, data=ant_test, var.equal=TRUE)
obs2 <- nortt2$statistic

res2 <- numeric(9999)
for (i in 1:9999) {
  perm2 <- sample(nrow(ant_test))
  pdat2 <-transform(ant_test,prop_dark=prop_dark[perm2])
  tt2 <- t.test(prop_dark~group, data=pdat2, car.equal=TRUE)
  res2[i] <- tt2$statistic
}
res2[2]
res2[3]

hist(res2,col="gray",las=1,main="") 
abline(v=obs2,col="red")
allvals2 <- c(res2, obs2)
mean(allvals2>=obs2)
(obs_pval2 <- 2*mean(allvals2>=obs2))
# it looks like the choice and no choice groups do not differ in efficiency with the 2 tailed

# Hypothesis 1 from text file: Colonies will improve performance with experience

# Test 3 - Regression for emigration duration over time
ant_train <- read_csv("ant_train.csv")
ant_train <- ant_train[ ,-(8:13)] # taking the easy way of removing values because this is a small subset of my data that I will not use for my actual stats

ant.lm <- lm(med_transport~training, data= ant_train)
summary(ant.lm)
# plot how median transport (in seconds) looks with experience (training sessions)
(ggplot(ant_train, aes(training,med_transport))+geom_point()+geom_smooth(method="lm", formula=y~x))

simfun2 <- function(duration="med_transport",data=ant_train) {
  permdat <- data
  permdat[[duration]] <- sample(permdat[[duration]])
  permdat
}
sumfun_train <- function(dat) {
  coef(lm(med_transport~training,data=dat))["training"]
}
permdist_train <- replicate(1999,sumfun_train(simfun2()))
(train_pval <- mean(abs(permdist_train)>=abs(sumfun_train(ant_train))))
hist(permdist_train,col="gray",breaks=30,freq=FALSE,main="")
abline(v=sumfun_train(ant_train),col="red") #### no line, could not get the scale to fit so the line is visible 

# ignore
# tried the general way but I didn't quite understand it, so did t-test instead ^
library("gtools")
comb <- combinations(nrow(ant_test), sum(ant_test$group=="Choice"))
nrow(comb)
simfun <- function(cc){
  transform(ant_test,decision_lat=c(decision_lat[cc],decision_lat[-cc]))
}
sumfun <- function(dat){ 
  t.test(decision_lat~group, data=ant_test, var.equal=TRUE)$statistic
}
ncomb <- nrow(comb)
permdist <- numeric(ncomb)
for (i in 1:ncomb) {
  permdist[i] <- sumfun(simfun(comb[i]))
}
s <- simfun(comb[1,])
sumfun(s)
obs_stat <- decisionPT$statistic



