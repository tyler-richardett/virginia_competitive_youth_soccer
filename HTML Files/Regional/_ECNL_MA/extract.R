library(rvest)
library(dplyr)

filenames <- list.files()
filenames <- filenames[-grep("extract.R", filenames)]
filenames <- filenames[-grep("csv$", filenames)]

schedule <- data.frame()

for (i in 1:length(filenames)) {
        Home <- read_html(filenames[i]) %>% html_nodes("table table table table table tr:nth-child(1) table td:nth-child(2)") %>% html_text(trim = TRUE)
        Away <- read_html(filenames[i]) %>% html_nodes("table table table table table tr:nth-child(2) table td:nth-child(2)") %>% html_text(trim = TRUE)
        Field <- read_html(filenames[i]) %>% html_nodes("table table table td:nth-child(3) table td") %>% html_text(trim = TRUE)
        tmp <- data.frame(Home, Away, Field, stringsAsFactors = FALSE)
        
        tmp$Age.Group <- rep(substring(filenames[i], 3, 5), nrow(tmp))
        tmp$Gender <- rep(ifelse(substring(filenames[i], 1, 1) == "B", "Boys", "Girls"), nrow(tmp))
        
        schedule <- rbind(schedule, tmp)
}

## Save full schedule.
## write.csv(schedule, "ECNL_MA_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% arrange(Home) %>% distinct()
fields <- schedule %>% filter(Field != "") %>% select(Field) %>% arrange(Field) %>% distinct()

schedule <- schedule %>% filter(Field != "") %>% select(Home, Away, Field)

## Save teams, fields, and schedule.
## write.csv(teams, "ECNL_MA_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "ECNL_MA_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "ECNL_MA_Schedule.csv", row.names = FALSE, na = "")