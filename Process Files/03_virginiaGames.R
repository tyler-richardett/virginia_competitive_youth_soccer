## Load necessary packages
library(dplyr)
library(tidyr)

## Import data
allGames <- read.csv("allGames.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 8), rep("numeric", 2), rep("character", 7)))
allFieldsCoordsTracts <- read.csv("allFieldsCoordsTracts.csv", stringsAsFactors = FALSE, na.strings = "", colClasses = c(rep("character", 7), rep("numeric", 2), "character"))

## Subset home games for Virginia clubs
virginiaGames <- allGames %>% filter(HomeClubState == "VA" | AwayClubState == "VA")

## Remove Development Academy showcases games
virginiaGames <- virginiaGames %>% filter(!(LeagueID == "LG0011" & FieldCity == "Advance" & FieldState == "NC"))
virginiaGames <- virginiaGames %>% filter(!(LeagueID == "LG0011" & FieldCity == "Lakewood Ranch" & FieldState == "FL"))
virginiaGames <- virginiaGames %>% filter(!(LeagueID == "LG0011" & FieldCity == "Frisco" & FieldState == "TX"))

## Remove Club Champions League college showcase games
virginiaGames <- virginiaGames %>% filter(!(LeagueID == "LG0004" & FieldID %in% allFieldsCoordsTracts$FieldID[grep("^RiverCity", allFieldsCoordsTracts$Name)]))

## Remove Club Champions League ScrimmageFest games
virginiaGames <- virginiaGames %>% filter(!(LeagueID == "LG0004" & Age.Group %in% c("U10", "U09") & FieldID %in% allFieldsCoordsTracts$FieldID[grep("^FFC Park", allFieldsCoordsTracts$Name)]))

## Separate into home and away
virginiaGamesHome <- virginiaGames %>% filter(HomeClubState == "VA")
virginiaGamesAway <- virginiaGames %>% filter(AwayClubState == "VA")

# write.csv(virginiaGames, "virginiaGames.csv", row.names = FALSE, na = "")
# write.csv(virginiaGamesHome, "virginiaGamesHome.csv", row.names = FALSE, na = "")
# write.csv(virginiaGamesAway, "virginiaGamesAway.csv", row.names = FALSE, na = "")