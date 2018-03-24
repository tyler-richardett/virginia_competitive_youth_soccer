library(rvest)
library(dplyr)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        Home <- read_html(filenames[i]) %>% html_nodes("td.schedtm1 span.tm-name") %>% html_text(trim = TRUE)
        Away <- read_html(filenames[i]) %>% html_nodes("td.schedtm2") %>% html_text(trim = TRUE)
        Field <- read_html(filenames[i]) %>% html_nodes("span.gm-fac") %>% html_text(trim = TRUE)
        tmp <- data.frame(Home, Away, Field, stringsAsFactors = FALSE)
        
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "BRSL"), Away = paste(Away, Age.Group, Gender, "BRSL"))

## Save full schedule.
## write.csv(schedule, "BRSL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "BRSL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "BRSL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "BRSL_Schedule.csv", row.names = FALSE, na = "")