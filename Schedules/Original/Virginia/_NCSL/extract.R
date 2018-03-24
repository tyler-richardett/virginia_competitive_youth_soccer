library(readxl)
library(dplyr)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

acronyms <- gsub("_schedule.xls", "", filenames)
        
schedule <- data.frame()

for (i in 1:length(filenames)) {
      tmp <- read_xls(filenames[i])
      tmp <- tmp %>% filter(grepl(acronyms[i], HOME_TEAM))
      
      schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% select(HOME_TEAM, HOME_TEAMCODE, AWAY_TEAM, DIVISION, FIELD)
schedule <- schedule %>% mutate(`Age Group` = substring(DIVISION, 2, 4), Gender = ifelse(grepl("B", HOME_TEAMCODE), "Boys", "Girls"))
schedule <- schedule %>% select(Home = HOME_TEAM, Away = AWAY_TEAM, `Age Group`, Gender, Field = FIELD)
schedule <- schedule %>% mutate(Home = paste(Home, `Age Group`, Gender, "NCSL"), Away = paste(Away, `Age Group`, Gender, "NCSL"))

## Save full schedule.
## write.csv(schedule, "NCSL_Full.csv", row.names = FALSE)

teams <- schedule %>% select(Home, `Age Group`, Gender) %>% distinct()
teams <- teams %>% arrange(Home)

fields <- schedule %>% select(Field) %>% distinct()
fields <- fields %>% arrange(Field)
fields <- fields %>% filter(!is.na(Field))

schedule <- schedule %>% select(Home, Away, Field)
schedule <- schedule %>% filter(!is.na(Field))

## Save teams, fields, and schedule.
## write.csv(teams, "NCSL_Teams.csv", row.names = FALSE)
## write.csv(fields, "NCSL_Fields.csv", row.names = FALSE)
## write.csv(schedule, "NCSL_Schedule.csv", row.names = FALSE)