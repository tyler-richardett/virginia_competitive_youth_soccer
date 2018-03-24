library(rvest)
library(dplyr)
library(tidyr)

Field <- read_html("Girls.html") %>% html_nodes("span.game-list-right-location") %>% html_children()
Field <- as.character(Field)
Field <- data.frame(Field, stringsAsFactors = FALSE)
Field$Field <- gsub('^<span.+\\">', "", Field$Field)
Field$Field <- gsub('</span>$', "", Field$Field)

row.nums <- NA

for (i in 1:(nrow(Field)-1)) {
        nextrow <- i+1
        fieldnumber <- i+2
        if(Field$Field[nextrow] == "") {
                Field$Field[i] <- paste(Field$Field[i], Field$Field[fieldnumber])
                row.nums <- c(row.nums, nextrow, fieldnumber)
        }
}

row.nums <- row.nums[-1]
Field <- Field[-row.nums,]
Field <- data.frame(Field, stringsAsFactors = FALSE)

Address <- read_html("Girls.html") %>% html_nodes("td.game-list-map a") %>% html_attr("href")
Address <- data.frame(Address, stringsAsFactors = FALSE)
Address$Address <- gsub("https://www.google.com/maps\\?q=", "", Address$Address)
Address <- Address %>% separate(Address, c("Street Address", "City", "State", "Zip Code"), ", ")

Field_Address <- cbind(Field, Address)
Field_Address$Field <- gsub("^ ", "", Field_Address$Field)
Field_Address$Field <- gsub(" $", "", Field_Address$Field)
Field_Address$Field <- gsub("&amp;", "&", Field_Address$Field)
Field_Address <- Field_Address %>% distinct() %>% arrange(Field)

Field$Field <- gsub("^ ", "", Field$Field)
Field$Field <- gsub(" $", "", Field$Field)
Field$Field <- gsub("&amp;", "&", Field$Field)

## write.csv(Field, "DA_Girls_Fields_GameOrder.csv", row.names = FALSE)
## write.csv(Field_Address, "DA_Girls_Fields_Addresses.csv", row.names = FALSE)

Girls <- read.csv("2017-2018 Girls Regular Season Schedule.csv", stringsAsFactors = FALSE)
Teams <- Girls %>% select(Team = Home) %>% distinct() %>% arrange(Team)
Teams <- Teams %>% mutate(Name = Team)
Teams <- Teams %>% separate(Name, c("Name", "Age Group"), "U-")
Teams$`Age Group` <- gsub("16/17", "17", Teams$`Age Group`)
Teams$`Age Group` <- gsub("18/19", "19", Teams$`Age Group`)
Teams$`Age Group` <- gsub(" Girls", "", Teams$`Age Group`)
Teams <- Teams %>% mutate(`Age Group` = paste0("U", `Age Group`))
Teams <- Teams %>% select(-Name)
Teams <- Teams %>% mutate(Gender = "Girls")

## write.csv(Teams, "DA_Girls_Teams.csv", row.names = FALSE)

Girls <- read.csv("2017-2018 Girls Regular Season Schedule.csv", stringsAsFactors = FALSE)
schedule <- Girls %>% select(Home, Away = Visitor, Field = Location)
schedule <- schedule %>% mutate(Home = paste(Home, "USDA"), Away = paste(Away, "Girls USDA"))

## write.csv(schedule, "DA_Girls_Schedule.csv", row.names = FALSE, na = "")

Boys <- read.csv("2017-2018 Boys Regular Season Schedule.csv", stringsAsFactors = FALSE)
schedule <- Boys %>% select(Home, Away = Visitor, Field = Location)
schedule <- schedule %>% mutate(Home = paste(Home, "USDA"), Away = paste(Away, "Boys USDA"))
schedule <- schedule %>% filter(!grepl("^(Toronto|Montreal|Vancouver)", Home))
schedule <- schedule %>% filter(!grepl("^TBD", Field))

## write.csv(schedule, "DA_Boys_Schedule.csv", row.names = FALSE, na = "")