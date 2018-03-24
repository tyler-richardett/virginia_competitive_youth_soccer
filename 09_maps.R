## Load necessary packages
library(dplyr)
library(leaflet)

## Import data
virginiaClubs <- read.csv("virginiaClubs.csv", na.strings = "", colClasses = c(rep("character", 2), rep("numeric", 3)))
load("virginiaTractsShapefiles.Rdata")
load("virginiaOutline.Rdata")

## Create Leaflet map for poverty
pal1 <- colorBin("Greys", virginiaTractsShapefiles$Poverty.Rate, 5, na.color = NA)

virginiaClubs %>% leaflet() %>%
        addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(opacity = 0.75)) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels") %>%
        addPolygons(data = virginiaTractsShapefiles, 
                    fillColor = ~pal1(Poverty.Rate), 
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.75, 
                    smoothFactor = 0.2) %>%
        addPolygons(data = virginiaOutline,
                    fillColor = NA,
                    fillOpacity = 0,
                    color = "#b2aeae",
                    weight = 1.25,
                    opacity = 1) %>% 
        addCircleMarkers(weight = 0.75, 
                         radius = log(virginiaClubs$Teams)*3,
                         fillColor = "#ff7256",
                         color = "#ffffff",
                         fillOpacity = 0.9,
                         opacity = 1,
                         popup = virginiaClubs$Name) %>% 
        addLegend(title = "Poverty Rate<br>(Families with Children Under 18)",
                  pal = pal1, 
                  values = virginiaTractsShapefiles$Poverty.Rate, 
                  position = "bottomleft", 
                  labFormat = labelFormat(suffix = "%"))


## Create Leaflet map for race
pal2 <- colorBin("Greys", virginiaTractsShapefiles$Minority.Percent, 5, na.color = NA)

virginiaClubs %>% leaflet() %>%
        addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(opacity = 0.75)) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels") %>%
        addPolygons(data = virginiaTractsShapefiles, 
                    fillColor = ~pal2(Minority.Percent), 
                    color = "#b2aeae",
                    fillOpacity = 0.7, 
                    weight = 0.75, 
                    smoothFactor = 0.2) %>%
        addPolygons(data = virginiaOutline,
                    fillColor = NA,
                    fillOpacity = 0,
                    color = "#b2aeae",
                    weight = 1.25,
                    opacity = 1) %>% 
        addCircleMarkers(weight = 0.75, 
                         radius = log(virginiaClubs$Teams)*3,
                         fillColor = "#ff7256",
                         color = "#ffffff",
                         fillOpacity = 0.9,
                         opacity = 1,
                         popup = virginiaClubs$Name) %>% 
        addLegend(title = "Share of Nonwhite Residents",
                  pal = pal2, 
                  values = virginiaTractsShapefiles$Minority.Percent, 
                  position = "bottomleft", 
                  labFormat = labelFormat(suffix = "%"))


## Create Leaflet map for poverty
pal3 <- colorFactor("Greys", virginiaTractsShapefiles$HHI.Factor, na.color = NA)

virginiaClubs %>% leaflet() %>%
        addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(opacity = 0.75)) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels") %>%
        addPolygons(data = virginiaTractsShapefiles, 
                    fillColor = ~pal3(HHI.Factor), 
                    color = "#ffffff",
                    fillOpacity = 0.7, 
                    weight = 0.75, 
                    smoothFactor = 0.2) %>%
        addPolygons(data = virginiaOutline,
                    fillColor = "#000000",
                    fillOpacity = 0,
                    color = "#b2aeae",
                    weight = 1.25,
                    opacity = 1) %>% 
        addCircleMarkers(weight = 0.75, 
                         radius = log(virginiaClubs$Teams)*3,
                         fillColor = "#ff7256",
                         color = "#ffffff",
                         fillOpacity = 0.9,
                         opacity = 1,
                         popup = virginiaClubs$Name) %>% 
        addLegend(title = "Median Household Income (2016 Dollars)",
                  pal = pal3, 
                  values = virginiaTractsShapefiles$HHI.Factor, 
                  position = "bottomleft")