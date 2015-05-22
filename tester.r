
library(dplyr)
library(ggplot2)

#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="repdata-data-StormData.csv.bz2", mode="wb")
filteredData <- tbl_df(read.csv(bzfile("repdata-data-StormData.csv.bz2"))) %>%   
  select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES) %>%
  filter(PROPDMG > 0 || CROPDMG > 0 || FATALITIES > 0 || INJURIES >0 )

#Manage the units contained in PROPDMGEXP and CROPDGMEXP
filteredData$CROPDMGEXP <- gsub("K","1000", filteredData$CROPDMGEXP)
filteredData$CROPDMGEXP <- gsub("M","1000000", filteredData$CROPDMGEXP)
filteredData$CROPDMGEXP <- gsub("B","1000000000", filteredData$CROPDMGEXP)
filteredData$CROPDMGEXP <- as.numeric(filteredData$CROPDMGEXP)
filteredData$CROPDMGEXP[is.na(filteredData$CROPDMGEXP)] = 0

filteredData$PROPDMGEXP <- gsub("K","1000", filteredData$PROPDMGEXP)
filteredData$PROPDMGEXP <- gsub("M","1000000", filteredData$PROPDMGEXP)
filteredData$PROPDMGEXP <- gsub("B","1000000000", filteredData$PROPDMGEXP)
filteredData$PROPDMGEXP <- as.numeric(filteredData$PROPDMGEXP)
filteredData$PROPDMGEXP[is.na(filteredData$PROPDMGEXP)] = 0

#Roll up the numbers
filteredData <- mutate(filteredData, damageTemp = (CROPDMG*CROPDMGEXP) + (PROPDMG*PROPDMGEXP))
filteredData <- mutate(filteredData, humanTemp = FATALITIES+INJURIES)

#Sum up the values
filteredData <- filteredData %>% group_by(EVTYPE) %>%
  summarize(damage=sum(damageTemp),human=sum(humanTemp))

#reduce the data to just the useful values
filteredData <- select(filteredData, EVTYPE, damage, human) %>% filter(damage >0 & human >0)
  
damageMax <- head(select(filteredData, EVTYPE, damage) %>% arrange(filteredData, -human)), 5)
  
ggplot(damageMax, aes(x=reorder(EVTYPE,-damage), y=damage/1000000000)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 5 Damaging Event Types") +
  xlab("Event Type") + ylab("Total Damage Amounts ($ Billions)") + 
  theme(axis.text.x = element_text(angle = 45))

humanMax <- head(select(filteredData, EVTYPE, human) %>% arrange(filteredData, -human)), 5)

ggplot(humanMax, aes(x=reorder(EVTYPE,-human), y=human)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top 5 Event Types Impacting Humans") +
  xlab("Event Type") + ylab("Total Fatalities+Deaths") + 
  theme(axis.text.x = element_text(angle = 45))