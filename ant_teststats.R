# Stats for replicates 1&2 tests

# cleaning data
# ant_testdata
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
# there's 5 NA's, so must delete empty rows
view(ant_test) #row 21-25
ant_test <- ant_test[-(21:25), ]
view(ant_test)

# one way ANOVA
# assumptions: Barlett for variance
bartlett.test(decision_lat~group,data=ant_test) # smaller than .05
bartlett.test(prop_dark~group,data=ant_test) # larger than .05
bartlett.test(cohesion~group,data=ant_test) # larger than .05
# assumption: Shapiro Wilk for normality
shapiro.test(ant_test$decision_lat) # smaller than .05
shapiro.test(ant_test$prop_dark) # smaller than .05
shapiro.test(ant_test$cohesion) # smaller than .05
# Kruskal wallace for decision latency
kruskal.test(decision_lat~group,data=ant_test)

