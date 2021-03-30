
#### Assingment 7 ####
# Generalized Linear Model 
# I don't have comments from my last 2 assingments yet so hopefully I don't keep making the same mistakes

## JD: I definitely wrote comments on your permutation assignment; sorry if you didn't get them! I feel it's my fault.

## I have this in my notes: 
## JD: Not very concrete to me what you're going to do. I definitely don't understand the PERMANOVA part. What's your single test statistic?
## I don't know if there was anything else, sorry!

library(tidyverse)
library(dplyr)
library(janitor)
train.data <- read.csv("ant_train.csv")
train.data <-remove_empty(train.data, which = c("rows", "cols")) 
print(train.data) # NoChoice group should have NAs for prop_dark

# Hypothesis: both groups improve performance as they gain experience

# Prediction 1: prop_dark will increase over time in the Choice group (not measured in NoChoice)
## use binomial because it's a proportion, measured between 0 to 1

## JD: Binomial wants successes and failures (0/1 outcomes), not proportions.
## I also get an error from R saying exactly that.
## Please advise

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



