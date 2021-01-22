ant_train <- read.csv("ant_train.csv")
print(ant_train)
## delete empty columns ##
ant_train <- ant_train[ ,-(8:13)]
print(ant_train)
## find the mean median transports of each group ##
## must remove NAs with na.rm=TRUE ##
tapply(ant_train$med_transport, INDEX = ant_train$group, FUN = mean, na.rm=TRUE)
## try a t-test ##
