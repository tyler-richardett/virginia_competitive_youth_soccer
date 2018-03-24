## Load necessary packages
library(tigris)

## Import census tract demographic data
virginiaTracts <- read.csv("virginiaTracts.csv", colClasses = c("character", rep("numeric", 9), "factor"))

## Import virginia census tract shapefiles
virginiaTractsShapefiles <- tracts("VA", cb = TRUE)

## Merge
virginiaTractsShapefiles <- geo_join(virginiaTractsShapefiles, virginiaTracts, "GEOID", "GEOID")

# save(virginiaTractsShapefiles, file = "virginiaTractsShapefiles.Rdata")