# MSc Project Stats

library(tidyverse)
library(dplyr)
library(ggplot2)

#clean data
#visualize data
#stat test

# Trying tests for Tricia # wilcoxon rank sum, kendals tau
testdata <- read_csv("testdatas1s2.csv")
if (require(janitor)) {
  testdata <- read_csv("testdatas1s2.csv")
  testdata <- remove_empty(testdata,which=c("rows","cols"))
  empty <- apply(testdata,1,
                 function(x) {
                   y <- unlist(x)
                   all(is.na(y) || nchar(y)==0)
                 })
  testdata <- testdata[!empty,]
} 
print(testdata)
# change margins if too large
par(mar=c(1,1,1,1))

plotdecision <- ggplot(testdata, aes(group, decision_lat, colour=group)) +geom_boxplot()
plotdecision
plotdirecttrans <- ggplot(testdata, aes(group, direct_dark, colour=group)) +geom_boxplot()
plotdirecttrans

wt.decision <- wilcox.test(decision_lat~group, data = testdata)
wt.decision
wt.directtrans <- wilcox.test(direct_dark~group, data = testdata, exact = FALSE) 
wt.directtrans


tt.decision <- t.test(decision_lat~group, data = testdata)
tt.efficiency <- t.test(direct_dark~group, data = testdata)

hist(testdata$decision_lat)
hist(testdata$direct_dark)

library(ggplot2)
theme_set(theme_classic(base_size=35))
DL <- ggplot(testdata, aes(group, decision_lat)) +geom_boxplot(alpha=0.3) + labs(y="Decision latency (seconds)") + theme(legend.position="none")+ stat_sum(alpha=0.3)+ scale_size(range=c(4,5))
DL
E <- ggplot(testdata, aes(group, direct_dark))+geom_boxplot(alpha=0.3) + labs(y="Proportion of transports to the better nest") + theme(legend.position="none") +stat_sum(alpha=0.3)+ scale_size(range=c(5,6))
E


##### TRICIA
ant_data<-read.csv("testdatas1s2.csv")
#ggplot stuff
install.packages("tidyverse")
library(ggplot2)
#rough boxplots
boxplot(ant_data$decision_lat~ant_data$group, xlab="Group", ylab="Decision Latency (s)", main="Decision Latency")
boxplot(ant_data$direct_dark~ant_data$group, xlab="Group", ylab="Proportion of Transports", main="Proportion of Direct Transports to the Dark Nest")
boxplot(ant_data$cohesion~ant_data$group, xlab="Group", ylab="Degree of Splitting", main="Cohesion")

latency<-ggplot(data=ant_data,aes(x=group,y=decision_lat))+geom_boxplot()
latency+theme_classic()+labs(
  y="Decision Latency (s)", 
  x="Group", 
  title="Decision Latency")

transports<-ggplot(data=ant_data,aes(x=group,y=direct_dark))+geom_boxplot()
transports+theme_classic()+labs(
  y="Proportion of Transports",
  x="Group",
  title="Proportion of Transports Directly to the Dark nest")

cohesion<-ggplot(data=ant_data,aes(x=group,y=cohesion))+geom_boxplot()
cohesion+theme_classic()+labs(
  y="Degree of Splitting Between Nests",
  x="Group",
  title="Colony Cohesiveness")

#histograms for all groups, visualize normality
hist(ant_data$decision_lat)
hist(ant_data$direct_dark)
hist(ant_data$cohesion, breaks=15)

#wilk shapiro test for normality
shapiro.test(ant_data$decision_lat)
shapiro.test(ant_data$direct_dark)
shapiro.test(ant_data$cohesion)

#wilcox test is the non-parametric equivalent to the t-test
wilcox.test(ant_data$decision_lat~ant_data$group)
wilcox.test(ant_data$direct_dark~ant_data$group, exact=FALSE)
wilcox.test(ant_data$cohesion)
            
