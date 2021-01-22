ant_train <- read.csv("ant_train.csv")
print(ant_train)
## delete empty columns ##
ant_train <- ant_train[ ,-(8:13)]
print(ant_train)
## find the mean median transports of each group ##
## must remove NAs with na.rm=TRUE ##
tapply(ant_train$med_transport, INDEX = ant_train$group, FUN = mean, na.rm=TRUE)
## try a t-test ##
cm <- 5705.552; sdc <- 1 # choice mean
ncm <- 5434.131; sdnc <- 1 # no choice mean
cn <- 23 # choice sample size
ncn <- 22 # no choice sample size
d1 <- rnorm(cn,cm,sdc)
d2 <- rnorm(ncn,ncm,sdnc)
t.test(d2,d1,paired=F,alternative="two.sided")