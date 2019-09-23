library(stringr)
library(dplyr)
library(tidyr)
setwd("~/CUNY/DATA 607 Data Acquisition and Management/project 1")
#Importing Tournament text file
projectraw <- read.delim("tournamentinfo.txt")
#Making copy of text file as a back up
rawcopy <- projectraw
#Removing empty rows 
projectnorows<-projectraw[-c(seq(3,195, by=3)),]
#Extracting first and last names. Each first name is longer than 2 letters so making the first name at least 3 letters lets us miss the 2 letter state.
projectnames1 <- unlist(str_extract_all(projectnorows, "[A-Z]{2,}([\\s]?[A-Z]*-?)*([^\\s|/])"))
#Removing the extracted column name
projectnames <- projectnames1[-1]
#Extracting states using the vertical lines to stop phrase
projectstatedirty <- unlist(str_extract_all(projectnorows, "[A-Z]{2}[\\s]\\|"))
#Removing the extra space and vertical line
projectstate <- gsub('.{2}$', '', projectstatedirty)
#Extracting points, each as a decimal we can use to idenifty them
projectpoints <- unlist(str_extract_all(projectnorows, "\\d\\.\\d"))
#Extracting pre-rating using 'R:' to identify them
projectpreratingdirty <- unlist(str_extract_all(projectnorows, "R:\\s*\\d*[^\\s-[:alpha:]]"))
#Removing R, :, and spaces
projectprerating <- gsub('^.{3}', '', projectpreratingdirty)
#Extracting list of opponents
projectopponentsdirty <- unlist(str_extract_all(projectnorows, "\\..*?$"))
#Removing letters, lines, and spaces
projectopponentsdirty2 <- gsub('^.{3}', '', projectopponentsdirty)
projectopponentsdirty3 <- gsub('\\|', '', projectopponentsdirty2)
projectopponentsdirty4 <- gsub('[WLDBHUX]', '', projectopponentsdirty3)
projectopponentsdirty5 <- gsub('[ ]{1,}', ',', projectopponentsdirty4)
projectopponents <- gsub('^.{1}', '', projectopponentsdirty5)
#Putting finished extracted variables into data frame
project <- data.frame(projectnames, projectstate, projectpoints, projectprerating)
#Putting dirty extracted variables into data frame
projectdirty <- data.frame(projectnames, projectstate, projectpoints, projectprerating, projectopponents)

#separate concatenated opponents into variables
projectdirty1 <- separate(projectdirty, projectopponents, c("opponent1", "opponent2", "opponent3", "opponent4", "opponent5", "opponent6", "opponent7"), sep = ",")

#replace na in opponents with 0
projectdirty2 <- replace(projectdirty1,is.na(projectdirty1),0)

#making opponents numeric in separate variables
opponent1 <- as.numeric(as.character(projectdirty2$opponent1))
opponent2 <- as.numeric(as.character(projectdirty2$opponent2))
opponent3 <- as.numeric(as.character(projectdirty2$opponent3))
opponent4 <- as.numeric(as.character(projectdirty2$opponent4))
opponent5 <- as.numeric(as.character(projectdirty2$opponent5))
opponent6 <- as.numeric(as.character(projectdirty2$opponent6))
opponent7 <- as.numeric(as.character(projectdirty2$opponent7))
numericopponents <- data.frame(opponent1, opponent2, opponent3, opponent4, opponent5, opponent6, opponent7)

#preping data for use as a row number where 0 = 1
numericopponents1 <- numericopponents - 1

#make preratingsum by summing prerating numbers from rows numbered by numericopponents1 value 
numericopponents2 <- mutate(numericopponents1, projectpreratingsum1 = projectdirty2$projectprerating[(row_number(opponent1))])
numericopponents3 <- mutate(numericopponents2, projectpreratingsum2 = projectdirty2$projectprerating[(row_number(opponent2))])
numericopponents4 <- mutate(numericopponents3, projectpreratingsum3 = projectdirty2$projectprerating[(row_number(opponent3))])
numericopponents5 <- mutate(numericopponents4, projectpreratingsum4 = projectdirty2$projectprerating[(row_number(opponent4))])
numericopponents6 <- mutate(numericopponents5, projectpreratingsum5 = projectdirty2$projectprerating[(row_number(opponent5))])
numericopponents7 <- mutate(numericopponents6, projectpreratingsum6 = projectdirty2$projectprerating[(row_number(opponent6))])
numericopponents8 <- mutate(numericopponents7, projectpreratingsum7 = projectdirty2$projectprerating[(row_number(opponent7))])

#making sums numeric
projectpreratingsum1 <- as.numeric(as.character(numericopponents8$projectpreratingsum1))
projectpreratingsum2 <- as.numeric(as.character(numericopponents8$projectpreratingsum2))
projectpreratingsum3 <- as.numeric(as.character(numericopponents8$projectpreratingsum3))
projectpreratingsum4 <- as.numeric(as.character(numericopponents8$projectpreratingsum4))
projectpreratingsum5 <- as.numeric(as.character(numericopponents8$projectpreratingsum5))
projectpreratingsum6 <- as.numeric(as.character(numericopponents8$projectpreratingsum6))
projectpreratingsum7 <- as.numeric(as.character(numericopponents8$projectpreratingsum7))

#Data frame of scores to be summed
projectpreratingsum <- data.frame(projectpreratingsum1, projectpreratingsum2, projectpreratingsum3, projectpreratingsum4, projectpreratingsum5, projectpreratingsum6, projectpreratingsum7)

projectpreratingsum1 <- replace(projectpreratingsum,is.na(projectpreratingsum),0)

#Summing pre rating scores
tobesummed <- c("projectpreratingsum1", "projectpreratingsum2", "projectpreratingsum3", "projectpreratingsum4", "projectpreratingsum5", "projectpreratingsum6", "projectpreratingsum7")
project$prerattingsum <- rowSums(projectpreratingsum1[, tobesummed])

#Deleting and renaming columns
project = subset(project, select = -c(opponent1, opponent2, opponent3, opponent4, opponent5, opponent6, opponent7) )
names(project) = c("Names", "State", "Points", "Pre-rating", "Sum of Opponenets Pre-Rating")

#Create CSV to be used in SQL
write.csv(project, "~/CUNY/DATA 607 Data Acquisition and Management/project 1/project1.csv")

#Check CSV
read.csv("~/CUNY/DATA 607 Data Acquisition and Management/project 1/project1.csv")
