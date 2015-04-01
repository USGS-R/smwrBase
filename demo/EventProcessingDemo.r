# Deomstrate the Event Processing functions
# This demonstation script illustrates some capabilities of the
# event-rpcoessing functions in smwrBase: eventNum, eventLen, and
# eventSeq
library(smwrBase)
library(dataRetrieval)
# Get one month of unit value precipitation data
# The site is G01-R 149N44W30CAAD 0000620661 in northwestern Minn.
G01precip <- readNWISuv("474135096203001", "00045",
  startDate="2012-06-01", endDate="2012-06-30",
  tz="America/Chicago")
# Manually rename the code and value column
names(G01precip)[5:6] <- c("Precip_cd", "Precip")
# What's there?
table(G01precip$Precip)
# Identify each precipitation event--each contiguous hour of rainfall
# Each contiguous rainfall event is assigned a unique, sequential number
G01precip <- transform(G01precip, Rain=eventNum(Precip > 0, reset=TRUE))
# How many events?
max(G01precip$Rain)
# How much rainfall each event? Rain event 0 is no precip
with(G01precip, tapply(Precip, Rain, sum))
# What are the durations for each event?
eventLen(G01precip$Rain, summary=TRUE)
