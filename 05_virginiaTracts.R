## Load necessary packages
library(dplyr)
library(tidyr)

## Import poverty rates for Virginia census tracts
virginiaTractsPoverty <- read.csv("ACS16_Poverty_FamiliesUnder18.csv", stringsAsFactors = FALSE, na.strings = c("-", "**", "(X)", ""), colClasses = c(rep("character", 3), rep("numeric", 20), rep("character", 580)))
virginiaTractsPoverty <- virginiaTractsPoverty %>% select(GEO.id2, HC02_EST_VC02) %>% dplyr::rename(GEOID = GEO.id2, Poverty.Rate = HC02_EST_VC02) %>% mutate(Poverty.Rate = as.numeric(Poverty.Rate))

## Import race information, calculate minority percentages for Virginia census tracts
virginiaTractsRace <- read.csv("ACS16_Race.csv", stringsAsFactors = FALSE, na.strings = c("*****", ""), colClasses = c(rep("character", 3), rep("numeric", 42)))
virginiaTractsRace <- virginiaTractsRace %>% 
        select(GEO.id2, HD01_VD01, HD01_VD03, HD01_VD04, HD01_VD05, HD01_VD06, HD01_VD07, HD01_VD12) %>% 
        dplyr::rename(GEOID = GEO.id2, Total = HD01_VD01, White = HD01_VD03, Black = HD01_VD04, American.Indian = HD01_VD05, Asian = HD01_VD06, Pacific.Islander = HD01_VD07, Hispanic = HD01_VD12) %>% 
        mutate(White.Percent = (White/Total)*100, Minority.Percent = (1-(White/Total))*100, Black.Percent = (Black/Total)*100, American.Indian.Percent = (American.Indian/Total)*100, Asian.Percent = (Asian/Total)*100, Pacific.Islander.Percent = (Pacific.Islander/Total)*100, Hispanic.Percent = (Hispanic/Total)*100) %>% 
        select(GEOID, White.Percent:Hispanic.Percent)

## Import HHI for Virginia census tracts
virginiaTractsMedianHHI <- read.csv("ACS16_MedianHHI.csv", stringsAsFactors = FALSE, na.strings = c("-", "**", ""))
virginiaTractsMedianHHI <- virginiaTractsMedianHHI %>% select(GEO.id2, HD01_VD01) %>% dplyr::rename(GEOID = GEO.id2, Household.Income = HD01_VD01) %>% mutate(GEOID = as.character(GEOID), Household.Income = as.numeric(Household.Income))
virginiaTractsMedianHHI$HHI.Factor <- cut(virginiaTractsMedianHHI$Household.Income, c(0, 25000, 50000, 75000, 100000, max(virginiaTractsMedianHHI$Household.Income, na.rm = TRUE)), labels = c("Under $25,000", "$25,000-49,999", "$50,000-74,999", "$75,000-99,999", "$100,000+"))

## Merge all
virginiaTracts <- virginiaTractsRace %>% left_join(virginiaTractsPoverty) %>% left_join(virginiaTractsMedianHHI)

# write.csv(virginiaTracts, "virginiaTracts.csv", row.names = FALSE, na = "")