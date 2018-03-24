## Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(gmapsdistance)
library(measurements)

## Import data
virginiaGames <- read.csv("virginiaGames.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 8), rep("numeric", 2), rep("character", 7)))
allTeams <- read_xlsx("Full Dataset.xlsx", "Teams")
virginiaClubs <- read.csv("virginiaClubs.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 2), rep("numeric", 3)))

## Count games per team
virginiaTeamsCost <- allTeams %>% filter(!is.na(Cost))
virginiaGamesTeamsCostHome <- virginiaGames %>% filter(HomeTeamID %in% virginiaTeamsCost$TeamID) %>% count(HomeTeamID) %>% dplyr::rename(TeamID = HomeTeamID, HomeGamesCount = n)
virginiaGamesTeamsCostAway <- virginiaGames %>% filter(AwayTeamID %in% virginiaTeamsCost$TeamID) %>% count(AwayTeamID) %>% dplyr::rename(TeamID = AwayTeamID, AwayGamesCount = n)
virginiaGamesTeamsCost <- virginiaGamesTeamsCostHome %>% full_join(virginiaGamesTeamsCostAway, "TeamID")
virginiaGamesTeamsCost <- virginiaGamesTeamsCost %>% mutate(Games = rowSums(select(., contains("Count")), na.rm = TRUE)) %>% select(TeamID, Games)
virginiaTeamsCost <- virginiaTeamsCost %>% left_join(virginiaGamesTeamsCost)

## Remove outliers
virginiaTeamsCost <- virginiaTeamsCost %>% filter(Games < 14 & Games > 4)

## Filter games with teams of interest as away
virginiaGamesTeamsTravel <- virginiaGames %>% filter(AwayTeamID %in% virginiaTeamsCost$TeamID) %>% select(GameID, AwayClubID, AwayTeamID, FieldLatitude, FieldLongitude)

## Join club location information
virginiaClubsKey <- virginiaClubs %>% select(AwayClubID = ClubID, ClubLatitude = Latitude, ClubLongitude = Longitude)
virginiaGamesTeamsTravel <- virginiaGamesTeamsTravel %>% left_join(virginiaClubsKey) %>% filter(!is.na(FieldLatitude) & !is.na(FieldLongitude) & !is.na(ClubLatitude) & !is.na(ClubLongitude))
virginiaGamesTeamsTravel <- virginiaGamesTeamsTravel %>% mutate(Origin = paste0(as.character(round(ClubLatitude, 5)), "+", as.character(round(ClubLongitude, 5))), Destination = paste0(as.character(FieldLatitude), "+", as.character(FieldLongitude)))

## Use Google Maps API to calculate distance traveled
virginiaGamesGMaps <- data.frame()

for(i in 1:nrow(virginiaGamesTeamsTravel)) {
        tmp <- gmapsdistance(virginiaGamesTeamsTravel$Origin[i], virginiaGamesTeamsTravel$Destination[i], mode = "driving")
        tmp <- data.frame(Time = tmp$Time, Distance = tmp$Distance, Status = tmp$Status, stringsAsFactors = FALSE)
        virginiaGamesGMaps <- rbind(virginiaGamesGMaps, tmp)
}

## Convert second to minutes, meters to miles
virginiaGamesGMaps <- virginiaGamesGMaps %>% mutate(Time = Time/60, Distance = conv_unit(Distance, "m", "mi")) %>% select(Time, Distance)

## Add time and distance to data frame with team IDs
virginiaGamesTeamsTravel <- cbind(virginiaGamesTeamsTravel, virginiaGamesGMaps)

## Summarize total travel by team
virginiaGamesTeamsTravel <- virginiaGamesTeamsTravel %>% group_by(AwayTeamID) %>% summarize(Time = sum(Time), Distance = sum(Distance)) %>% dplyr::rename(TeamID = AwayTeamID)
virginiaGamesTeamsTravel <- virginiaGamesTeamsTravel %>% left_join(virginiaGamesTeamsCostAway)

## Join travel information and clean
virginiaTeamsCost <- virginiaTeamsCost %>% left_join(virginiaGamesTeamsTravel)
virginiaTeamsInvestment <- virginiaTeamsCost %>% select(TeamID, `Age Group`, Gender, LeagueID, TotalCost = Cost, CoachPaid, TotalGames = Games, TotalTime = Time, TotalDistance = Distance, AwayGames = AwayGamesCount) %>% filter(!is.na(TotalDistance)) %>% mutate(TotalTime = TotalTime*2, TotalDistance = TotalDistance*2)
virginiaTeamsInvestment <- virginiaTeamsInvestment %>% mutate(WeightedCost = round(((TotalCost*8)/TotalGames)/2, 2), WeightedTime = round((TotalTime*4)/AwayGames), WeightedDistance = round((TotalDistance*4)/AwayGames))

## Split by competition level
virginiaTeamsInvestment$LeagueTier <- NA
virginiaTeamsInvestment$LeagueTier[virginiaTeamsInvestment$LeagueID %in% c("LG0011", "LG0012")] <- "Tier 1"
virginiaTeamsInvestment$LeagueTier[virginiaTeamsInvestment$LeagueID %in% c("LG0010", "LG0006", "LG0004")] <- "Tier 2"
virginiaTeamsInvestment$LeagueTier[virginiaTeamsInvestment$LeagueID %in% c("LG0013")] <- "Tier 3"
virginiaTeamsInvestment$LeagueTier[virginiaTeamsInvestment$LeagueID %in% c("LG0001", "LG0002", "LG0003", "LG0005", "LG0007", "LG0009")] <- "Tier 4"
virginiaTeamsInvestment$LeagueTier[virginiaTeamsInvestment$LeagueID == "LG0008"] <- "Tier 5"
virginiaTeamsInvestment$LeagueTier <- as.factor(virginiaTeamsInvestment$LeagueTier)

# write.csv(virginiaTeamsInvestment, "virginiaTeamsInvestment.csv", row.names = FALSE, na = "")