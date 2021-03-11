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



