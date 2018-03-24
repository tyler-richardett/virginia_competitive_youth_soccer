library(dplyr)
library(rvest)

schedule <- data.frame()

for (i in 1:94) {
        tmp_url <- paste0("http://www.clubchampionsleague.com/schedules/page/", i, "/?season=11&club=0&group=0&gender=0&daterange=08%2F01%2F2017%20-%2012%2F31%2F2017&submit=Filter")
        
        Home <- read_html(tmp_url) %>% html_nodes("td.column-home_club div.home-club-cell div p.club-title") %>% html_text(trim = TRUE)
        Away <- read_html(tmp_url) %>% html_nodes("td.column-away_club div.away-club-cell div p.club-title") %>% html_text(trim = TRUE)
        Age.Group <- read_html(tmp_url) %>% html_nodes("td.column-group") %>% html_text(trim = TRUE)
        Field <- read_html(tmp_url) %>% html_nodes("td.column-location") %>% html_text(trim = TRUE)
        
        tmp <- data.frame(Home, Away, Age.Group, Field, stringsAsFactors = FALSE)
        schedule <- rbind(schedule, tmp)
}

for (i in 1:30) {
        tmp_url <- paste0("http://www.clubchampionsleague.com/schedules/page/", i, "/?season=12&club=0&group=0&gender=0&daterange=08%2F01%2F2017%20-%2012%2F31%2F2017&submit=Filter")
        
        Home <- read_html(tmp_url) %>% html_nodes("td.column-home_club div.home-club-cell div p.club-title") %>% html_text(trim = TRUE)
        Away <- read_html(tmp_url) %>% html_nodes("td.column-away_club div.away-club-cell div p.club-title") %>% html_text(trim = TRUE)
        Age.Group <- read_html(tmp_url) %>% html_nodes("td.column-group") %>% html_text(trim = TRUE)
        Field <- read_html(tmp_url) %>% html_nodes("td.column-location") %>% html_text(trim = TRUE)
        
        tmp <- data.frame(Home, Away, Age.Group, Field, stringsAsFactors = FALSE)
        schedule <- rbind(schedule, tmp)
}

schedule$Age.Group <- gsub("0male$", "0 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("1male$", "1 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("2male$", "2 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("3male$", "3 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("4male$", "4 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("5male$", "5 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("6male$", "6 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("7male$", "7 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("8male$", "8 Male", schedule$Age.Group)
schedule$Age.Group <- gsub("9male$", "9 Male", schedule$Age.Group)

schedule$Age.Group <- gsub("0female$", "0 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("1female$", "1 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("2female$", "2 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("3female$", "3 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("4female$", "4 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("5female$", "5 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("6female$", "6 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("7female$", "7 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("8female$", "8 Female", schedule$Age.Group)
schedule$Age.Group <- gsub("9female$", "9 Female", schedule$Age.Group)

schedule$Age.Group <- gsub("U9", "U09", schedule$Age.Group)

schedule$Field <- gsub("Field: ", " #", schedule$Field)
schedule$Field <- gsub("^TBA$", NA, schedule$Field)


schedule <- schedule %>% mutate(Home = paste(Home, Age.Group), Away = paste(Away, Age.Group), Gender = ifelse(grepl("Female$", Age.Group), "Girls", "Boys"))
schedule <- schedule %>% mutate(Age.Group = substring(Age.Group, 6, 8))

ccl <- schedule[1:1675,]
cclii <- schedule[1676:2213,]

ccl <- ccl %>% mutate(Home = paste(Home, "CCL"), Away = paste(Away, "CCL"))
cclii <- cclii %>% mutate(Home = paste(Home, "CCL II"), Away = paste(Away, "CCL II"))
schedule <- rbind(ccl, cclii)

## Save full schedule.
## write.csv(schedule, "CCL_Full.csv", row.names = FALSE, na = "")

teams <- schedule %>% select(Home, Age.Group, Gender) %>% distinct()
teams <- teams %>% arrange(Home)

fields <- schedule %>% select(Field) %>% distinct()
fields <- fields %>% arrange(Field)

schedule <- schedule %>% select(Home, Away, Field)
schedule <- schedule %>% filter(!is.na(Field))

## Save teams, fields, and schedule.
## write.csv(teams, "CCL_Teams.csv", row.names = FALSE, na = "")
## write.csv(fields, "CCL_Fields.csv", row.names = FALSE, na = "")
## write.csv(schedule, "CCL_Schedule.csv", row.names = FALSE, na = "")