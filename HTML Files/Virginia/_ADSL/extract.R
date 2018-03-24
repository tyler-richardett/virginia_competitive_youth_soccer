library(dplyr)
library(rvest)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        Home <- read_html(filenames[i]) %>% html_nodes("td.homeTeam") %>% html_text(trim = TRUE)
        Away <- read_html(filenames[i]) %>% html_nodes("td.awayTeam") %>% html_text(trim = TRUE)
        Field <- read_html(filenames[i]) %>% html_nodes("td.location") %>% html_text(trim = TRUE)
        tmp <- data.frame(Home, Away, Field, stringsAsFactors = FALSE)
        
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "ADSL"), Away = paste(Away, Age.Group, Gender, "ADSL"))

## Save full schedule.
## write.csv(schedule, "ADSL_Full.csv", row.names = FALSE)

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "ADSL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "ADSL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "ADSL_Schedule.csv", row.names = FALSE, na = "")