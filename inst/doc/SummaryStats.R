### R code from vignette source 'SummaryStats.Rnw'

###################################################
### code chunk number 1: SummaryStats.Rnw:30-40
###################################################
# Load the smwrBase and smwrData packages
library(smwrBase)
library(smwrData)
# Retrieve streamflow data for the  Choptank River near Greensboro, Maryland
data(ChoptankFlow)
# Print the first and last few rows of the data
head(ChoptankFlow)
tail(ChoptankFlow)
# Check for missing values
with(ChoptankFlow, screenData(datetime, Flow, year = "calendar"))


###################################################
### code chunk number 2: SummaryStats.Rnw:52-59
###################################################
# There are no missing values, so only need the basic 
# 3 arguments for tapply
ChoptankFlow.daily <- with(ChoptankFlow, tapply(Flow, 
    baseDay(datetime, numeric=FALSE, year="calendar"), mean))
# Print the first and last few values of the output
head(ChoptankFlow.daily)
tail(ChoptankFlow.daily)


###################################################
### code chunk number 3: SummaryStats.Rnw:66-75
###################################################
# There are no missing values
ChoptankFlow.dailyDF <- aggregate(Flow ~ 
    baseDay(datetime, numeric=FALSE, year="calendar"), 
    data=ChoptankFlow, FUN=mean)
# Print the first and last few values of the output
head(ChoptankFlow.dailyDF)
tail(ChoptankFlow.dailyDF)
# Rename the grouping column
names(ChoptankFlow.dailyDF)[1] <- "Day"


###################################################
### code chunk number 4: SummaryStats.Rnw:85-93
###################################################
# There are no missing values
ChoptankFlow.yrDF <- aggregate(Flow ~ 
    year(datetime), 
    data=ChoptankFlow, FUN=mean)
# Rename the grouping column
names(ChoptankFlow.yrDF)[1] <- "CalYear"
# Print the first few values of the output
head(ChoptankFlow.yrDF)


###################################################
### code chunk number 5: SummaryStats.Rnw:103-110
###################################################
# There are no missing values
ChoptankFlow.my <- aggregate(Flow ~ month(datetime, label=TRUE) + year(datetime), 
    data=ChoptankFlow, FUN=mean)
# Rename columns 1 and 2
names(ChoptankFlow.my)[1:2] <- c("Month", "Year")
# Print the first few values of the output
head(ChoptankFlow.my)


###################################################
### code chunk number 6: SummaryStats.Rnw:115-120
###################################################
# Restructure the dataset
ChoptankFlow.myTbl <- group2row(ChoptankFlow.my, "Year", "Month", "Flow")
# Print the first few values of the output, set width for Vignette
options(width=70)
head(ChoptankFlow.myTbl)


