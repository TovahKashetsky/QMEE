### Classwork and testing shit out ###

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


