## Load necessary packages
library(dplyr)
library(tidyr)

## Import data
allFieldsCoordsTracts <- read.csv("allFieldsCoordsTracts.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 7), rep("numeric", 2), "character"))
virginiaTracts <- read.csv("virginiaTracts.csv", colClasses = c("character", rep("numeric", 9), "factor"))

## Isolate census tracts containing fields in Virginia
virginiaFieldsCoordsTracts <- allFieldsCoordsTracts %>% filter(State == "VA")
virginiaTractsFields <- data.frame(GEOID = unique(virginiaFieldsCoordsTracts$GEOID))

## Merge
virginiaTractsFields <- virginiaTractsFields %>% mutate(GEOID = as.character(GEOID)) %>% left_join(virginiaTracts)

# write.csv(virginiaTractsFields, "virginiaTractsFields.csv", row.names = FALSE, na = "")