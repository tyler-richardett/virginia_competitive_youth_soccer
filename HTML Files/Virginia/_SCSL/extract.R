library(dplyr)
library(rvest)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        tmp <- read_html(filenames[i]) %>% html_node("table.datatable") %>% html_table(fill = TRUE)
        tmp <- tmp %>% select(-`Date/Time`)
        tmp <- tmp %>% mutate(Team2 = Game) %>% rename(Team1 = Game)
        
        tmp$Team1 <- gsub("\\s+v\\..+$", "", tmp$Team1)
        tmp$Team2 <- gsub("^.+v\\.\\s+", "", tmp$Team2)
        tmp$Team2 <- gsub("\\s+\\(.+\\)$", "", tmp$Team2)
        tmp$Field <- gsub("(^z- |^z - )", "", tmp$Field)
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

teams1 <- schedule %>% select(Team1, Age.Group, Gender) %>% arrange(Team1) %>% rename(Team = Team1) %>% distinct()
teams2 <- schedule %>% select(Team2, Age.Group, Gender) %>% arrange(Team2) %>% rename(Team = Team2) %>% distinct()
teams <- rbind(teams1, teams2)
teams <- teams %>% distinct()

fields <- schedule %>% filter(!is.na(Field)) %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(!is.na(Field)) %>% select(Team1, Field, Team2)
