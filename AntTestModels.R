# Stats for replicates 1&2 tests
# trying out statistic models 

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
