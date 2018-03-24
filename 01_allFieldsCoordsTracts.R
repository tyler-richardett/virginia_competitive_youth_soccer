## Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(RDSTK)
library(stringr)
library(censusr)

## Import data from Excel file
allFields <- read_xlsx("Full Dataset.xlsx", "Fields")

## Generate latitudes and longitudes from field addresses
allFields <- allFields %>% mutate(Address = paste0(`Street Address`, ", ", City, ", ", State, " ", `Zip Code`))

latsLongs <- data.frame()

for(i in 1:length(allFields$Address)) {
        tmp <- street2coordinates(allFields$Address[i])
        tmp$FieldID <- allFields$FieldID[i]
        latsLongs <- rbind(latsLongs, tmp)
}

latsLongs <- latsLongs %>% select(FieldID, lat = latitude, lon = longitude)
allFieldsCoords <- allFields %>% left_join(latsLongs, "FieldID")

## Generate GEOIDs from latitudes and longitudes
allFieldsCoordsTracts <- allFieldsCoords
allFieldsCoordsTracts$GEOID <- append_geoid(allFieldsCoords[,8:9])$geoid
allFieldsCoordsTracts <- allFieldsCoordsTracts %>% dplyr::select(Name:Address, Latitude = lat, Longitude = lon, GEOID) %>% dplyr::mutate(GEOID = substring(GEOID, 1, 11))

# write.csv(allFieldsCoordsTracts, "allFieldsCoordsTracts.csv", row.names = FALSE, na = "")