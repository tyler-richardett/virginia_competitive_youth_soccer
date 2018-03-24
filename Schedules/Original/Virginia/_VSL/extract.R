library(dplyr)
library(rvest)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        tmp <- read_html(filenames[i]) %>% html_node("form table table table") %>% html_table(fill = TRUE)
        tmp <- tmp %>% filter(grepl("^#", X1)) %>% select(Home = X3, Away = X5, Field = X7)
        
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "VSL"), Away = paste(Away, Age.Group, Gender, "VSL"))

## Save full schedule.
## write.csv(schedule, "VSL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "VSL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "VSL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "VSL_Schedule.csv", row.names = FALSE, na = "")