library(readxl)
library(dplyr)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

getCode <- function(df) {
        df <- df %>% mutate(Acronym = substring(HOME_TEAM, 1, 4))
        code <- names(sort(table(df$Acronym), decreasing = TRUE)[1])
        code
}

for (i in 1:length(filenames)) {
        tmp <- read_xls(filenames[i])
        tmp <- tmp %>% filter(grepl(paste0("^", getCode(tmp)), HOME_TEAM))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% select(HOME_TEAM, HOME_TEAMCODE, AWAY_TEAM, DIVISION, FIELD)
schedule <- schedule %>% mutate(`Age Group` = substring(DIVISION, 1, 3), Gender = ifelse(grepl("B", HOME_TEAMCODE), "Boys", "Girls"))
schedule <- schedule %>% select(Home = HOME_TEAM, Away = AWAY_TEAM, `Age Group`, Gender, Field = FIELD)
schedule$`Age Group` <- gsub("HS ", "U19", schedule$`Age Group`)
schedule$`Age Group` <- gsub("U15", "U16", schedule$`Age Group`)
schedule <- schedule %>% mutate(Home = paste(Home, `Age Group`, Gender, "ODSL"), Away = paste(Away, `Age Group`, Gender, "ODSL"))

## Save full schedule.
## write.csv(schedule, "ODSL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, `Age Group`, Gender) %>% distinct()
teams <- teams %>% arrange(Home)

fields <- schedule %>% select(Field) %>% distinct()
fields <- fields %>% arrange(Field)
fields <- fields %>% filter(!is.na(Field))

schedule <- schedule %>% select(Home, Away, Field)
schedule <- schedule %>% filter(!is.na(Field))

## Save teams, fields, and schedule.
## write.csv(teams, "ODSL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "ODSL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "ODSL_Schedule.csv", row.names = FALSE, na = "")