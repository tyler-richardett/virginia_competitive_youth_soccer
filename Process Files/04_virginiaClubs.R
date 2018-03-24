## Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)

## Import data
virginiaGamesHome <- read.csv("VirginiaGamesHome.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 8), rep("numeric", 2), rep("character", 7)))
allTeams <- read_xlsx("Full Dataset.xlsx", "Teams")

## Function that will calculate distance in km from geographic mean to each coordinate. Credit: https://stackoverflow.com/questions/24439073/removing-spatial-outliers-lat-and-long-coordinates-in-r
earth.dist <- function (long1, lat1, long2, lat2) {
        rad <- pi/180
        a1 <- lat1 * rad
        a2 <- long1 * rad
        b1 <- lat2 * rad
        b2 <- long2 * rad
        dlon <- b2 - a2
        dlat <- b1 - a1
        a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        R <- 6378.145
        d <- R * c
        return(d)
}


## Calculate geographic mean and remove outliers (>= 50 km from center)
virginiaClubsKey <- virginiaGamesHome %>% group_by(HomeClubID) %>% dplyr::summarize(Name = unique(HomeClubName), ClubAvgLatitude = mean(FieldLatitude, na.rm = TRUE), ClubAvgLongitude = mean(FieldLongitude, na.rm = TRUE))
outliersKey <- virginiaGamesHome %>% left_join(virginiaClubsKey, "HomeClubID")
outliersKey <- outliersKey %>% group_by(HomeClubID, FieldID) %>% dplyr::summarize(Distance = earth.dist(FieldLongitude, FieldLatitude, ClubAvgLongitude, ClubAvgLatitude)[1])
outliers <- outliersKey %>% filter(Distance >= 50)

for(i in 1:nrow(outliers)){
        virginiaGamesHome <- virginiaGamesHome %>% filter(!(HomeClubID == outliers$HomeClubID[i] & FieldID == outliers$FieldID[i]))
}


## Calculate number of teams per club
allClubsCounts <- allTeams %>% group_by(ClubID) %>% dplyr::summarize(Teams = length(TeamID))
virginiaClubs <- virginiaGamesHome %>% group_by(HomeClubID) %>% dplyr::summarize(Name = unique(HomeClubName), Latitude = mean(FieldLatitude, na.rm = TRUE), Longitude = mean(FieldLongitude, na.rm = TRUE)) %>% dplyr::rename(ClubID = HomeClubID)
virginiaClubs <- virginiaClubs %>% left_join(allClubsCounts)

# write.csv(virginiaClubs, "virginiaClubs.csv", row.names = FALSE, na = "")