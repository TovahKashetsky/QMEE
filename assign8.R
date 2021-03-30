## Assingment 8
library(tidyverse)
ant_test <- read_csv("tests1s2.csv")
library(arm)
library(R2jags)
library(coda)
library(broom.mixed)
library(emmeans)
library(dotwhisker)
library(ggplot2) # R was telling me I needed most of the packages but it might've been lying

named_list <- lme4:::namedList

# how big is the difference in decision latency between my groups
bayes.data <- with(ant_test, 
                    named_list(N=nrow(ant_test), # total obs
                               ngroup=length(group),  # number categories
                               group=as.numeric(group), # numeric index
                               decision_lat)) # decision
# make bugs model
bugs.model <- function() {
  for (i in 1:N){
    y[i] ~ dnorm(pred[i], tau)
    pred[i] <- ngroup*decision_lat[i] + int
  }
  ngroup ~ dnorm (0, 0.0001)
  int ~ dnorm(0, 0.0001)
  tau ~ dgamma(0.001, 0.001)  
}
# setting up jags
bayes.jags <-  jags(data=bayes.data,
                    inits=NULL,
                    model.file=bugs.model,
                    parameters=c("b_group", "int", "tau"))

broom.mixed::tidy(bayes.jags,conf.int=TRUE, conf.method="quantile")

# compare the two models 
F.model <- lm(decision_lat~group, data=ant_test)
summary(F.model) # frequentist model
B.model <- bayesglm(decision_lat~group, data=ant_test)
summary(B.model) # bayes model
# identical results between the regular linear model and the bayes GLM

# plotting
dotwhisker::dwplot(list(flat=F.model,shrunk=B.model)) + geom_vline(xintercept=0,lty=2)
# both flat and shrunk model CIs intersect iwth zero
# not much difference between models 
traceplot(bayes.jags)
# my int plot is messy, doesn't seem like there's any clear patterns 
# my tau plot has large variation in lines, could mean my model is illfit
