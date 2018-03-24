library(dplyr)
library(rvest)

filenames <- list.files()
filenames <- filenames[-grep("R$", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

## Used this to add class names to the table rows.
## for (i in 1:length(filenames)) {
##         tmp <- readLines(filenames[i])
##         tmp <- gsub('tr bgcolor="#f5f5f5"', 'tr bgcolor="#f5f5f5" class="gameS"', tmp)
##         tmp <- gsub('tr bgcolor="#ffffff"', 'tr bgcolor="#ffffff" class="games2"', tmp)
##         writeLines(tmp, filenames[i])
## }

schedule <- data.frame()

for (i in 1:length(filenames)) {
        tmp.home <- read_html(filenames[i]) %>% html_nodes("tr.gameS td:nth-child(3)") %>% html_text(trim = TRUE)
        tmp.away <- read_html(filenames[i]) %>% html_nodes("tr.gameS td:nth-child(5)") %>% html_text(trim = TRUE)
        tmp.fields <- read_html(filenames[i]) %>% html_nodes("tr.gameS td:nth-child(7)") %>% html_text(trim = TRUE)
        
        tmp <- data.frame(Home = tmp.home, Away = tmp.away, Field = tmp.fields, stringsAsFactors = FALSE)
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

schedule <- schedule %>% mutate(Home = paste(Home, Age.Group, Gender, "EDP"), Away = paste(Away, Age.Group, Gender, "EDP"))

## Save full schedule.
## write.csv(schedule, "EDP_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "Unassigned" & Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "EDP_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "EDP_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "EDP_Schedule.csv", row.names = FALSE, na = "")


#####

schedule2 <- data.frame()

for (i in 1:length(filenames)) {
        tmp.home <- read_html(filenames[i]) %>% html_nodes("tr.games2 td:nth-child(3)") %>% html_text(trim = TRUE)
        tmp.away <- read_html(filenames[i]) %>% html_nodes("tr.games2 td:nth-child(5)") %>% html_text(trim = TRUE)
        tmp.fields <- read_html(filenames[i]) %>% html_nodes("tr.games2 td:nth-child(7)") %>% html_text(trim = TRUE)
        
        tmp <- data.frame(Home = tmp.home, Away = tmp.away, Field = tmp.fields, stringsAsFactors = FALSE)
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule2 <- rbind(schedule2, tmp)
}

schedule2 <- schedule2 %>% mutate(Home = paste(Home, Age.Group, Gender, "EDP"), Away = paste(Away, Age.Group, Gender, "EDP"))

## Save full schedule.
## write.csv(schedule, "EDP_Full2.csv", row.names = FALSE, na = "")

teams2 <- schedule2 %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields2 <- schedule2 %>% filter(Field != "Unassigned" & Field != "" & Field != "TBD TBD") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule2 <- schedule2 %>% filter(Field != "Unassigned" & Field != "" & Field != "TBD TBD") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "EDP_Teams2.csv", row.names = FALSE, na = "")
## write.csv(fields, "EDP_Fields2.csv", row.names = FALSE, na = "")
## write.csv(schedule, "EDP_Schedule2.csv", row.names = FALSE, na = "")