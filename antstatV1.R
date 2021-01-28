ant_train <- read.csv("ant_train.csv")
print(ant_train)

## delete empty columns ##
## JD: maybe better to fix this on the spreadsheet side; numbers are always dicey
ant_train <- ant_train[ ,-(8:13)]
print(ant_train)

## JD: This is fine for now, but you will live a happier life if you avoid tapply ☺
## find the mean median transports of each group ##
## must remove NAs with na.rm=TRUE ##
tapply(ant_train$med_transport, INDEX = ant_train$group, FUN = mean, na.rm=TRUE)

## try a t-test ##
## JD: avoid typing numbers that you've calculated
cm <- 5705.552; sdc <- 1 # choice mean
ncm <- 5434.131; sdnc <- 1 # no choice mean
cn <- 23 # choice sample size
ncn <- 22 # no choice sample size

## JD: This is pretty wild: you are doing a t-test on made-up numbers (rnorm) instead of the real ones.
d1 <- rnorm(cn,cm,sdc)
d2 <- rnorm(ncn,ncm,sdnc)
t.test(d2,d1,paired=F,alternative="two.sided")


=======
  ## Grade 2/2
  >>>>>>> f948d4deca33e0ddf47ab9ec080b52655521f27c


<<<<<<< HEAD
## doing a t test for the test phase ##
test.dec <- read.csv("testdecisions.csv")
print(test.dec)
## delete empty rows and columns ##
test.dec <- test.dec[ -(21:92),-(4:13)]
print(test.dec)
## find the mean decision latency of each group ##
tapply(test.dec$decision_lat, INDEX = test.dec$group, FUN = mean,)





