
# ASSIGNMENT 1 #

ant_train <- read.csv("ant_train.csv")
print(ant_train)
## BMB: it helps if you can work on/clean up/respond to comments as you go
## and delete comments that have been resolved

## delete empty columns ##
## JD: maybe better to fix this on the spreadsheet side; numbers are always dicey
## BMB: or use janitor::remove_empty() ?
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
## TK: I used my old notes for a t-test but clearly didn't undertsand that function
d1 <- rnorm(cn,cm,sdc)
d2 <- rnorm(ncn,ncm,sdnc)
t.test(d2,d1,paired=F,alternative="two.sided")



## Grade 2/3



# IGNORE #
## tried a t test for the test phase ##
test.dec <- read.csv("testdecisions.csv")
print(test.dec)
## BMB:
if (require(janitor)) {
    test.dec <- read.csv("testdecisions.csv")
    test.dec <- remove_empty(test.dec,which=c("rows","cols"))
    ## unfortunately janitor doesn't find rows that are blank OR NA
    empty <- apply(test.dec,1,
                   function(x) {
                       y <- unlist(x)
                       all(is.na(y) || nchar(y)==0)
                   })
    test.dec <- test.dec[!empty,]
}

## delete empty rows and columns ##
test.dec <- test.dec[ -(21:92),-(4:13)]


print(test.dec)
## find the mean decision latency of each group ##
tapply(test.dec$decision_lat, INDEX = test.dec$group, FUN = mean,)


# ASSIGNMENT 2 #
library(tidyverse)

str(ant_train)
## not sure if I should convert "training" into a factor
## I want to know how the colonies changed over time through each training session 
## so I am keeping it as an integer for now

## BMB: that's probably sensible

## should be unique combinations of colony and training
print(ant_train %>% group_by(colony, training) %>% summarize(count = n())) 
print(ant_train %>% group_by(colony, training) %>% summarize(count = n()) %>% filter(count>1))

## BMB: nice, consider stopifnot()/assertthat


## relational table to make sure each colony has 6 training counts
print(ant_train %>% group_by(colony, training)%>% summarize(count=n())%>% group_by(colony)%>% summarize(count=n())%>% filter(count>1)%>% arrange(desc(count)))

## check plots for decison latency and median transports
## I expect they'll look similar
print(ggplot(ant_train, aes(x=med_transport))+ geom_histogram())
print(ggplot(ant_train, aes(x=decision_latency))+ geom_histogram())

## BMB: could pivot_longer() and overlay them ...

## Grade: 2/3
