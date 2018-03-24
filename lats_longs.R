## Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(RDSTK)
library(tigris)
library(censusr)
library(stringr)
library(ggplot2)
library(leaflet)


## Import data from Excel file
all.teams <- read_xlsx("Full Dataset.xlsx", "Teams")
all.games <- read_xlsx("Full Dataset.xlsx", "Games")
all.clubs <- read_xlsx("Full Dataset.xlsx", "Clubs")
all.fields <- read_xlsx("Full Dataset.xlsx", "Fields")
all.leagues <- read_xlsx("Full Dataset.xlsx", "Leagues")


## Generate latitudes and longitudes from field addresses
all.fields <- all.fields %>% mutate(Address = paste0(`Street Address`, ", ", City, ", ", State, " ", `Zip Code`))

lats.longs <- data.frame()

for(i in 1:length(all.fields$Address)) {
        tmp <- street2coordinates(all.fields$Address[i])
        tmp$FieldID <- all.fields$FieldID[i]
        lats.longs <- rbind(lats.longs, tmp)
}

lats.longs.only <- lats.longs %>% select(FieldID, latitude, longitude)
all.fields.coords <- all.fields %>% left_join(lats.longs.only, "FieldID")
        # write.csv(all.fields.coords, "all_fields_coords.csv", row.names = FALSE, na = "")

## Keep only necessary information to subset games
all.fields.key <- all.fields.coords %>% select(FieldID, FieldCity = City, FieldState = State, FieldLatitude = latitude, FieldLongitude = longitude)
all.teams.key <- all.teams %>% select(HomeTeamID = TeamID, Age.Group = `Age Group`, Gender, LeagueID)
all.clubs.key.home <- all.clubs %>% select(HomeClubID = ClubID, HomeClubName = Name, HomeClubState = State)
all.clubs.key.away <- all.clubs %>% select(AwayClubID = ClubID, AwayClubName = Name, AwayClubState = State)

## Merge and clean games
all.games <- all.games %>% left_join(all.fields.key, "FieldID") %>% left_join(all.teams.key, "HomeTeamID") %>% left_join(all.clubs.key.home, "HomeClubID") %>% left_join(all.clubs.key.away, "AwayClubID")
        # write.csv(all.games, "all_games.csv", row.names = FALSE, na = "")
virginia.home.games <- all.games %>% filter(HomeClubState == "VA")

# Remove Development Academy showcases games
virginia.home.games <- virginia.home.games %>% filter(!(LeagueID == "LG0011" & FieldCity == "Advance" & FieldState == "NC"))
virginia.home.games <- virginia.home.games %>% filter(!(LeagueID == "LG0011" & FieldCity == "Lakewood Ranch" & FieldState == "FL"))
virginia.home.games <- virginia.home.games %>% filter(!(LeagueID == "LG0011" & FieldCity == "Frisco" & FieldState == "TX"))

# Remove Club Champions League college showcase games
virginia.home.games <- virginia.home.games %>% filter(!(LeagueID == "LG0004" & FieldID %in% all.fields.coords$FieldID[grep("^RiverCity", all.fields.coords$Name)]))

# Remove Club Champions League ScrimmageFest games
virginia.home.games <- virginia.home.games %>% filter(!(LeagueID == "LG0004" & Age.Group %in% c("U10", "U09") & FieldID %in% all.fields.coords$FieldID[grep("^FFC Park", all.fields.coords$Name)]))

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
virginia.club.locations.key <- virginia.home.games %>% group_by(HomeClubID) %>% dplyr::summarize(Name = unique(HomeClubName), ClubAvgLatitude = mean(FieldLatitude), ClubAvgLongitude = mean(FieldLongitude))
outliers.key <- virginia.home.games %>% left_join(virginia.club.locations.key, "HomeClubID")
outliers.key <- outliers.key %>% group_by(HomeClubID, FieldID) %>% dplyr::summarize(Distance = earth.dist(FieldLongitude, FieldLatitude, ClubAvgLongitude, ClubAvgLatitude)[1])
outliers <- outliers.key %>% filter(Distance >= 50)

for(i in 1:nrow(outliers)){
        virginia.home.games <- virginia.home.games %>% filter(!(HomeClubID == outliers$HomeClubID[i] & FieldID == outliers$FieldID[i]))
}


## Calculate number of teams per club
virginia.club.team.counts <- all.teams %>% group_by(ClubID) %>% dplyr::summarize(Teams = length(TeamID))
virginia.club.locations <- virginia.home.games %>% group_by(HomeClubID) %>% dplyr::summarize(Name = unique(HomeClubName), Latitude = mean(FieldLatitude, na.rm = TRUE), Longitude = mean(FieldLongitude, na.rm = TRUE)) %>% dplyr::rename(ClubID = HomeClubID)
virginia.club.locations <- virginia.club.locations %>% left_join(virginia.club.team.counts)


## Download shapefiles of Virginia census tracts
virginia <- tracts("VA", cb = TRUE)


## Import poverty rates for Virginia census tracts and merge
poverty.virginia <- read.csv("ACS16_Poverty_FamiliesUnder18.csv", stringsAsFactors = FALSE, na.strings = c("-", "**"))
poverty.virginia <- poverty.virginia[-c(1,2),]
poverty.virginia <- poverty.virginia %>% select(GEO.id2, HC02_EST_VC02) %>% dplyr::rename(GEOID = GEO.id2, Poverty.Rate = HC02_EST_VC02) %>% mutate(Poverty.Rate = as.numeric(Poverty.Rate))

poverty.tracts.virginia <- geo_join(virginia, poverty.virginia, "GEOID", "GEOID")


## Create Leaflet map for poverty
pal1 <- colorBin("Greys", poverty.tracts.virginia$Poverty.Rate, 5, na.color = NA)

virginia.club.locations %>% leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = poverty.tracts.virginia, 
                    fillColor = ~pal1(Poverty.Rate), 
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addCircleMarkers(weight = 0, 
                         radius = log(virginia.club.locations$Teams)*3,
                         fillColor = "#ff7256",
                         fillOpacity = 0.9, 
                         popup = virginia.club.locations$Name) %>% 
        addLegend(title = "Share of Families Living in Poverty",
                  pal = pal1, 
                  values = poverty.tracts.virginia$Poverty.Rate, 
                  position = "bottomleft", 
                  labFormat = labelFormat(suffix = "%"))


## Import race information, calculate minority percentages for Virginia census tracts, and merge
race.virginia <- read.csv("ACS_16_5YR_B02001_with_ann.csv", stringsAsFactors = FALSE, na.strings = "*****")
race.virginia <- race.virginia[-1,]
race.virginia <- race.virginia %>% select(GEO.id2, HD01_VD01, HD01_VD02) %>% dplyr::rename(GEOID = GEO.id2, Total = HD01_VD01, White = HD01_VD02) %>% mutate(Total = as.numeric(Total), White = as.numeric(White)) %>% mutate(Nonwhite.Percent = (1-(White/Total))*100) %>% select(GEOID, Nonwhite.Percent)

race.tracts.virginia <- geo_join(virginia, race.virginia, "GEOID", "GEOID")


## Create Leaflet map for race
pal2 <- colorBin("Greys", race.tracts.virginia$Nonwhite.Percent, 5, na.color = NA)

virginia.club.locations %>% leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = race.tracts.virginia, 
                    fillColor = ~pal2(Nonwhite.Percent), 
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addCircleMarkers(weight = 0, 
                         radius = log(virginia.club.locations$Teams)*3,
                         fillColor = "#ff7256",
                         fillOpacity = 0.9, 
                         popup = virginia.club.locations$Name) %>% 
        addLegend(title = "Share of Nonwhite Residents",
                  pal = pal2, 
                  values = race.tracts.virginia$Nonwhite.Percent, 
                  position = "bottomleft", 
                  labFormat = labelFormat(suffix = "%"))


## Import HHI for Virginia census tracts and merge
hhi.virginia <- read.csv("ACS_16_5YR_B19013_with_ann.csv", stringsAsFactors = FALSE, na.strings = c("-", "**"))
hhi.virginia <- hhi.virginia[-c(1,2),]
hhi.virginia <- hhi.virginia %>% select(GEO.id2, HD01_VD01) %>% dplyr::rename(GEOID = GEO.id2, Household.Income = HD01_VD01) %>% mutate(Household.Income = as.numeric(Household.Income))
hhi.virginia$Factor <- cut(hhi.virginia$Household.Income, c(0, 25000, 50000, 75000, 100000, max(hhi.virginia$Household.Income, na.rm = TRUE)), labels = c("Under $25,000", "$25,000-49,999", "$50,000-74,999", "$75,000-99,999", "$100,000+"))

hhi.tracts.virginia <- geo_join(virginia, hhi.virginia, "GEOID", "GEOID")

## Create Leaflet map for poverty
pal3 <- colorFactor("Greys", hhi.virginia$Factor, na.color = NA)

virginia.club.locations %>% leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = hhi.tracts.virginia, 
                    fillColor = ~pal3(Factor), 
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addCircleMarkers(weight = 0, 
                         radius = log(virginia.club.locations$Teams)*3,
                         fillColor = "#ff7256",
                         fillOpacity = 0.9, 
                         popup = virginia.club.locations$Name) %>% 
        addLegend(title = "Median Household Income (2016 Dollars)",
                  pal = pal3, 
                  values = hhi.tracts.virginia$Factor, 
                  position = "bottomleft")