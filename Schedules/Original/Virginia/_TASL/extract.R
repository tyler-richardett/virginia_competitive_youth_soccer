library(dplyr)
library(rvest)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        tmp.teams.1 <- read_html(filenames[i]) %>% html_nodes("tr.trstyle1 td.trstyleGame") %>% html_text()
        tmp.fields.1 <- read_html(filenames[i]) %>% html_nodes("tr.trstyle1 td a") %>% html_attr("href")
        tmp.teams.2 <- read_html(filenames[i]) %>% html_nodes("tr.trstyle2 td.trstyleGame") %>% html_text()
        tmp.fields.2 <- read_html(filenames[i]) %>% html_nodes("tr.trstyle2 td a") %>% html_attr("href")
        
        tmp <- data.frame(Home = c(tmp.teams.1, tmp.teams.2), Away = c(tmp.teams.1, tmp.teams.2), Field = c(tmp.fields.1, tmp.fields.2))
        tmp$Home <- gsub("( vs.+$| played.+$|\\s+[0-9]+,.+$)", "", tmp$Home)
        tmp$Away <- gsub("(^.+vs\\. |^.+played |^.+[0-9]+,\\s+)", "", tmp$Away)
        tmp$Away <- gsub("CANCELLED", "", tmp$Away)
        tmp$Away <- gsub("\\s+[0-9]+.+$", "", tmp$Away)
        tmp$Field <- gsub("^javascript:directWindow\\('", "", tmp$Field)
        tmp$Field <- gsub("','.+$", "", tmp$Field)
        
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "TASL"), Away = paste(Away, Age.Group, Gender, "TASL"))

## Save full schedule.
## write.csv(schedule, "TASL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "TASL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "TASL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "TASL_Schedule.csv", row.names = FALSE, na = "")