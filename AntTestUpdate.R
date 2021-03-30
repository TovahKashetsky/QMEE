
#### Assignment 5 ####

library(tidyverse)
library(dplyr)
library(janitor)
ant_test <- read_csv("ant_testdata.csv")
ant_test <- rename(ant_test, video = colony_1) # two columns named colony, change colony_1 to video
print(ant_test)

# Hypothesis 2 from text file: choice group will perform better on a decision-making task than the no choice group

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
view(ant_train) # should still be NAs in prop_dark for the NoChoice group because there is no such measure during the training phase of the expierment for that group

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


#### Assignment 6 ####

# Hypothesis: choice group will perform better on a decision-making task than the no choice group
# same data as above, run if needed:
# ant_test <- read_csv("ant_testdata.csv")
# ant_test <- rename(ant_test, video = colony_1) # two columns named colony, change colony_1 to video

decision.lm <- lm(decision_lat~group, data=ant_test)
summary(decision.lm)
plot(decision.lm)
# model shows difference between groups is not significant
# residual vs fitted plot looks like line is almost straight which is good
# normal Q-Q suggests that my data point "5" might be an outlier. 
# no residual vs leverage plot, so I can't see if the 5 is within Cook's distance
# scale-location line is constant, so that's good

darktrans.lm <- lm(prop_dark~group, data=ant_test)
summary(darktrans.lm)
plot(darktrans.lm)
# model shows difference between groups is not significant (fingers crossed for when I have more data)
# residuals vs fitted plot is great, red line is nearly flat
# normal Q-Q is a little wonky
# no residual vs leverage plot
# scale-location line is constant, that's good

dummy.lm <- lm(decision_lat~prop_dark, data=ant_test)
summary(dummy.lm) 
plot(dummy.lm)
# this was to see how two continuous variables looked, both variables are dependent
# all diagnostic plots look good

library(emmeans)

decision.e <- emmeans(decision.lm,"group")
pairs(decision.e)
plot(decision.e)
# looks like the choice group has a lower emmean than the no choice group, which is expected
# there is a fair overlap though, which is consistant with my previous tests 
# there is more overlap compared to my ggplots print(plotdecision) from assingment 3
# plotdecision <- ggplot(ant_test, aes(group, decision_lat, colour=group)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")

darktrans.e <- emmeans(darktrans.lm,"group")
pairs(darktrans.e)
plot(darktrans.e)
# looks like the choice group has a higher emmean than the no choice group, which is also expected
# still some overlap, not as much though


#### Assingment 7 ####
# Generalized Linear Model 
# I don't have comments from my last 2 assingments yet so hopefully I don't keep making the same mistakes

library(tidyverse)
library(dplyr)
library(janitor)
train.data <- read.csv("ant_train.csv")
train.data <-remove_empty(train.data, which = c("rows", "cols")) 
print(train.data) # NoChoice group should have NAs for prop_dark

# Hypothesis: both groups improve performance as they gain experience

# Prediction 1: prop_dark will increase over time in the Choice group (not measured in NoChoice)
## use binomial because it's a proportion, measured between 0 to 1
train.choice <- train.data %>% group_by(group) %>% filter(group=="Choice") # make data only for choice group
print(train.choice)
pd <- ggplot(train.choice, aes(training,prop_dark))+geom_point()
pd1 <- pd + geom_smooth(method="glm",colour="orange",
                           formula=y~x,
                           method.args=list(family="binomial")) 
pdmodel <- glm(prop_dark~training, family=binomial, data=train.choice)
summary(pdmodel)
plot(pdmodel) 
# all of the diagnostic plots look good, possibly an outlier (5) that's right on Cooks distance
acf(residuals(pdmodel)) 
# see a slight pattern of residuals starting in positives then going negative

# Prediction 2: med_transport will decrease in both groups over time
## use gamma because it's counts with no max, non integers
mt <- ggplot(train.data, aes(training,med_transport))+geom_point()
mt1 <- mt + geom_smooth(method="glm",colour="orange",
                        formula=y~x,
                        method.args=list(family=Gamma(link="log"))) 
mtmodel <- glm(med_transport~training, family=Gamma(link="log"), data=train.data)
summary(mtmodel) 
plot(mtmodel)
# plots look fine ... red lines not as straight as they could be but no major issues
# looks better than when I incorrectly used the poisson family

# for the inferential stats I will do another model looking at med_transport between groups, instead of over training sessions
mtx <- ggplot(train.data, aes(group,med_transport))+geom_point()
mt1x <- mtx + geom_smooth(method="glm",colour="orange",
                        formula=y~x,
                        method.args=list(family=Gamma(link="log"))) 
mtxmodel <- glm(med_transport~group, family=Gamma(link="log"), data=train.data)
summary(mtxmodel) 
plot(mtxmodel) # diagnostic plots look fine
library(emmeans)
mtx.emmeans <- emmeans(mtxmodel,"group")
plot(mtx.emmeans) # NoChoice group has smaller emmean than Choice group. Makes sense because the nochoice group doesn't have the added time of making decisions

