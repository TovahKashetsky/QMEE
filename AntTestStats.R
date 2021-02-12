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