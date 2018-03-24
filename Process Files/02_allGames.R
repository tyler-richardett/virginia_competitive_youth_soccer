## Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)

## Import data from Excel file
allTeams <- read_xlsx("Full Dataset.xlsx", "Teams")
allGames <- read_xlsx("Full Dataset.xlsx", "Games")
allClubs <- read_xlsx("Full Dataset.xlsx", "Clubs")
allFieldsCoordsTracts <- read.csv("allFieldsCoordsTracts.csv", stringsAsFactors = FALSE, na.strings = "")

## Keep only necessary information to subset games
allFieldsKey <- allFieldsCoordsTracts %>% select(FieldID, FieldCity = City, FieldState = State, FieldLatitude = Latitude, FieldLongitude = Longitude)
allTeamsKey <- allTeams %>% select(HomeTeamID = TeamID, Age.Group = `Age Group`, Gender, LeagueID)
allClubsKeyHome <- allClubs %>% select(HomeClubID = ClubID, HomeClubName = Name, HomeClubState = State)
allClubsKeyAway <- allClubs %>% select(AwayClubID = ClubID, AwayClubName = Name, AwayClubState = State)

## Merge and clean games
allGames <- allGames %>% left_join(allFieldsKey, "FieldID") %>% left_join(allTeamsKey, "HomeTeamID") %>% left_join(allClubsKeyHome, "HomeClubID") %>% left_join(allClubsKeyAway, "AwayClubID")

# write.csv(allGames, "allGames.csv", row.names = FALSE, na = "")