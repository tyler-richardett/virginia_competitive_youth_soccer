library(rvest)
library(dplyr)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for(i in 1:length(filenames)) {
        Home <- read_html(filenames[i]) %>% html_nodes("td.homeTeam") %>% html_text(trim = TRUE)
        Away <- read_html(filenames[i]) %>% html_nodes("td.awayTeam") %>% html_text(trim = TRUE)
        Field <- read_html(filenames[i]) %>% html_nodes("td.location") %>% html_text(trim = TRUE)
        Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), length(Home))
        Age.Group <- rep(substring(filenames[i], 3, 5), length(Home))
        
        tmp <- data.frame(Home, Away, Age.Group, Gender, Field, stringsAsFactors = FALSE)
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "VA NPL"), Away = paste(Away, Age.Group, Gender, "VA NPL"))

## Save full schedule.
## write.csv(schedule, "NPL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "NPL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "NPL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "NPL_Schedule.csv", row.names = FALSE, na = "")