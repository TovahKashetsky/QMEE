
#### Assingment 7 ####

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
# TK: I changed prediction 1 to quasipossion 
## JD: This does not sound right. Why would quasipossion be good for proportions?
## Also, quasipossion should not be happy with non-integers either
train.choice <- train.data %>% group_by(group) %>% filter(group=="Choice") # make data only for choice group
print(train.choice)
pd <- ggplot(train.choice, aes(training,prop_dark))+geom_point()
pd1 <- pd + geom_smooth(method="glm",colour="orange",
                           formula=y~x,
                           method.args=list(family=quasipoisson(link="log"))) 

## JD: Not sure why you didn't show this plot; it's helpful.

print(pd1)

## JD: The correct way to model proportions is to base it on what it's a proportion _of_. 
## We talked about two ways to do this in class; either as successes and failures, or with proportions and weights
## Sorry that my previous message was short, but I thought you would figure it out.
## It looks to me tot_transports is your denominator, although it's hard to be sure.
## In any case, it's not surprising you don't see a clear pattern in these data.

goodmodel <- glm(data=train.choice, prop_dark~training
	, weights = tot_transports
	, family="binomial"
)
summary(goodmodel)

pdmodel <- glm(data=train.choice, prop_dark~training, family=quasipoisson(link="log"))
summary(pdmodel)
par(mar=c(1,1,1,1)) # margins too large
plot(pdmodel) 
# all of the diagnostic plots look good, I don't see any major issues. The normal q-q could be straighter ?
acf(residuals(pdmodel)) 
# see a slight pattern of residuals starting in positives then going negative

# Prediction 2: med_transport will decrease in both groups over time
## use gamma because it's counts with no max, non integers
## JD: I'm not sure what you mean by counts (counts usually mean discrete events, counted in integers).
mt <- ggplot(train.data, aes(training,med_transport))+geom_point()
mt1 <- mt + geom_smooth(method="glm",colour="orange",
                        formula=y~x,
                        method.args=list(family=Gamma(link="log")))

## JD: Looks like there might be an interesting pattern here
print(mt1)
print(ggplot(train.data)
	+ aes(training,med_transport, color=group)
	+ geom_point()
	+ geom_smooth(method="lm")
)

mtmodel <- glm(med_transport~training, family=Gamma(link="log"), data=train.data)
summary(mtmodel) 
plot(mtmodel)
# plots look fine ... red lines not as straight as they could be but no major issues
# normal q-q is a little wonky but I dont think it's an issue ? I think gamma is the correct model for my data
# looks better than when I incorrectly used the poisson family

# for the inferential stats I will do another model looking at med_transport between groups, instead of over training sessions
## JD: Why one or the other? Doesn't seem to match your question; was there a problem using both predictors?
mtx <- ggplot(train.data, aes(group,med_transport))+geom_point()
mt1x <- mtx + geom_smooth(method="glm",colour="orange",
                        formula=y~x,
                        method.args=list(family=Gamma(link="log"))) 
mtxmodel <- glm(med_transport~group, family=Gamma(link="log"), data=train.data)
summary(mtxmodel) 
plot(mtxmodel) # diagnostic plots look fine. Again normal q-q is a bit wonky but not innaproriately so
library(emmeans)
mtx.emmeans <- emmeans(mtxmodel,"group")
plot(mtx.emmeans) # NoChoice group has smaller emmean than Choice group. Makes sense because the nochoice group doesn't have the added time of making decisions

## JD: Not sure why you seem to think you've seen something clearly. Did you look at the model summary?

## Grade 1.6/3
