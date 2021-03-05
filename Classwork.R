### Classwork and testing stuff out ###

## data carpentry ecology link ##
download.file(url = "https://ndownloader.figshare.com/files/2292169", destfile = "/Users/faith/Desktop/data_raw/portal_data_joined.csv") 
library(tidyverse)
surveys <- read_csv("/Users/faith/Desktop/data_raw/portal_data_joined.csv")
head(surveys)
str(surveys)
nrow(surveys)
ncol(surveys)
surveys_200 <- surveys[200, ]

##### Jan 26 and 28 #####
## data analysis steps ##
# setwd("~/Desktop/PSYCH708/New/QMEE")
download.file(url = "https://raw.githubusercontent.com/mac-theobio/QMEE/master/docs/data/village.csv", destfile = "~/Desktop/PSYCH708/New/QMEE/village.csv")
villageTable <- read_csv("../QMEE/village.csv")
summary(villageTable)

# renaming column cyl to mr krabs
library(dplyr)
mtcars1 <- rename(mtcars, 
                  MrKrabs = cyl)

#renaming row Valiant to sausage
rownames(mtcars1) [rownames(mtcars1) == "Valiant"] = "sausage"
mtcars1

#### Feb 2 ####
library(tidyverse)
homdat <- read_csv("~/Desktop/PSYCH708/New/QMEE/CA_homicide.csv")
popdat <- read_csv("~/Desktop/PSYCH708/New/QMEE/CA_popdat.csv")

rdat <- tibble(Place=homdat$Place,
               Region=c("all",rep("Atlantic",4),
                        rep("East",2),
                        rep("West",4),
                        rep("North",3)))
head(rdat)

sdat <- (homdat %>% pivot_longer(names_to="year",values_to="homicide",-Place, names_transform=list(year=as.numeric)))
str(sdat)
sdat2 <- sdat %>% full_join(rdat,by="Place") %>% full_join(popdat,by="Place")
str(sdat2$Place)
sdat3 <- (sdat2 %>% mutate (Place=fct_reorder(Place,Pop_2011)))
str(sdat3$Place)

saveRDS(sdat3, file="CA_homicide.rds") 
mdat <- readRDS("CA_homicide.rds") 

library(ggplot2)
# set theme if u want
theme_set(theme_bw())
theme_set(base_size=10)
p1 <- ggplot(mdat,aes(year,homicide,colour=Place))
print (p1 + geom_line())
p2 <- ggplot(mdat, aes(year, homicide, colour=Place))+geom_line()+ labs(y="homicides per 100,000 population")
p2
print (p1 + geom_boxplot())
ggplot(mdat,aes(Place,homicide))+geom_boxplot()
# flip axes for better labels
p3 <- ggplot(mdat,aes(Place,homicide))+geom_boxplot()+coord_flip()
p3
# reorder
mdat_sort <- mdat %>% mutate(Place=fct_reorder(Place,homicide))
regiondat %+% mdat_sort + facet_wrap(Region)
p3
mdat_sort <- mdat %>% mutate(Place=fct_reorder(Place,homicide))

## Permutations ## Feb 24 ##

setwd("~/Desktop/PSYCH708/New/QMEE")
download.file(url = "https://raw.githubusercontent.com/mac-theobio/QMEE/master/docs/data/ants.csv", destfile = "~/Desktop/PSYCH708/New/QMEE/qmeeants.csv")
qmeeants <- read_csv("../QMEE/qmeeants.csv")

ggplot(qmeeants,aes(x=place,y=colonies)) + geom_boxplot()
set.seed(101)
res <- numeric(9999)
for (i in 1:9999) {
  
  perm <- sample(nrow(qmeeants))
  sample(nrow(qmeeants))
  bdat <- transform(qmeeants,colonies=colonies[perm])
  
  fieldmean <- mean(bdat[bdat$place=="field","colonies"])
  fieldmean
  forestmean <- mean(bdat[bdat$place=="forest","colonies"])
  forestmean
  res[i] <- fieldmean - forestmean
}
res[1]
res[2] 

truefieldmean <- qmeeants %>% filter(place=="field") %>% pull(colonies) %>% mean
trueforestmean <- qmeeants %>% filter(place=="forest") %>% pull(colonies) %>% mean
obsval <- truefieldmean - trueforestmean
hist(res)
abline(v=obsval, col="red") 
allvals <- c(res,obsval)
sum(allvals>=obsval)
mean(allvals>=obsval)
2*mean(allvals >=obsval)

# t test
tt <- t.test(colonies~place,data=qmeeants,var.equal=TRUE)
tt$statistic
tt <- t.test(colonies~place,data=bdat,var.equal=TRUE)
res[i] <- tt$statistic

# linear models

download.file(url = "https://raw.githubusercontent.com/mac-theobio/QMEE/master/docs/data/skewdat.csv", destfile = "~/Desktop/PSYCH708/New/QMEE/skewdat.csv")
skewdata <- read.csv("../QMEE/skewdat.csv")
m1 <- lm(skew~Size, data=skewdata)
plot(m1,id.n=6) #id is to see the numbers of the 6 largest residuals 

download.file(url = "https://raw.githubusercontent.com/mac-theobio/QMEE/master/docs/data/lizards.csv", destfile = "~/Desktop/PSYCH708/New/QMEE/lizards.csv")
lizards <- read.csv("../QMEE/lizards.csv")                                                               
lizards$time <- factor(lizards$time,levels=c("early","midday","late"))

lmint <- lm(grahami~light*time, data=lizards)
lmboth <- lm(grahami~light + time, data=lizards)
lmlight <- lm(grahami~light, data=lizards)
lmtime <- lm(grahami~time, data=lizards)
summary(lmint) 
summary(lmboth) 
summary(lmlight) 
summary(lmtime) 

drop1(lmboth, test="F")
car::Anova(lmboth)

library(multcomp)
summary(mc <- glht(lmboth, linfct=mcp(time="Tukey"))) 
library(emmeans)
e1 <- emmeans(lmboth, "time")
pairs(e1)

mc <- glht(lmboth, linfct=mcp(time="Tukey"))
summary(mc)
cld(mc)

summary(lm1 <- lm(colonies~place,data=qmeeants))
summary(lm2 <- lm(colonies~place-1,data=qmeeants))
summary(lm3 <- lm(colonies~place+0,data=qmeeants))



