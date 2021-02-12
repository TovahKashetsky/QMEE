# ASSINGMENT 3 #

library(tidyverse)
ant_test2 <- read_csv("ant_testdata.csv")
print(ant_test2)
# two columns named colony, change colony_1 to video
library(dplyr)
ant_test2 <- rename(ant_test2, video = colony_1)
print(ant_test2)
# make replicate and video a factor
ant_test2 <- (ant_test2 %>% mutate(replicate=as.factor(replicate)))
ant_test2 <- (ant_test2 %>% mutate(video=as.factor(video)))
summary(ant_test2)
# remove empty rows 
library(janitor)
remove_empty(ant_test2, which = c("rows", "cols")) 

# making ggplot
library(ggplot2)
plot1 <- ggplot(ant_test2, aes(group, decision_lat, colour=group))+geom_boxplot() 
# there's still a place in plot for NA so try the "bad" way to get rid of NA
ant_test3 <- ant_test2[-(21:26), ]
view(ant_test3)

# make the ggplots for decision, prop dark transports, and cohesion
theme_set(theme_bw())
# white background, make see through with alpha
plotdecision <- ggplot(ant_test3, aes(group, decision_lat, colour=group)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")
plotdark <- ggplot(ant_test3, aes(group, prop_dark, colour=group))+geom_boxplot(alpha=0.3) + labs(y="Proportion of transports to the dark nest") + theme(legend.position="none")
plotcohesion <- ggplot(ant_test3, aes(group, cohesion, colour=group))+geom_boxplot(alpha=0.3) + labs(y="Cohesiveness") + theme(legend.position="none")

# I like box plots for visualizing decision and cohesion, it's informative
# for plotdark, there's a wide range, so would dots be better to visualize it?
plotdark2 <- ggplot(ant_test3, aes(group, prop_dark, colour=group))+geom_point() + labs(y="Proportion of transports to the dark nest") + theme(legend.position="none")
# I like box plot better because it's clear the where the mean and quartiles are

# Plot colours
library(wesanderson)
names(wes_palettes)

theme_set(theme_classic(base_size=35))
plotdecision1 <- ggplot(ant_test3, aes(group, decision_lat)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")+ stat_sum(alpha=0.3)+ scale_size(range=c(4,5))
plotdark1 <- ggplot(ant_test3, aes(group, prop_dark))+geom_boxplot(alpha=0.3) + labs(y="Proportion of transports to the dark nest") + theme(legend.position="none") +stat_sum(alpha=0.3)+ scale_size(range=c(5,6))
plotcohesion1 <- ggplot(ant_test3, aes(group, cohesion))+geom_boxplot(alpha=0.3) + labs(y="Cohesiveness") + theme(legend.position="none")+stat_sum(alpha=0.3)+ scale_size(range=c(4,5))


