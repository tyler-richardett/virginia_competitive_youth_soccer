library(rvest)
library(dplyr)
library(tidyr)

Field <- read_html("http://events.gotsport.com/events/fields.aspx?eventid=59881") %>% html_nodes("blockquote table tr td:nth-child(1)") %>% html_text(trim = TRUE)
Address <- read_html("http://events.gotsport.com/events/fields.aspx?eventid=59881") %>% html_nodes("blockquote table tr td:nth-child(2)") %>% html_children()
Address <- as.character(Address)

field_address <- data.frame(Field, Address, stringsAsFactors = FALSE)
field_address <- field_address[-1,]
field_address$Address[grep("Not Available", field_address$Address)] <- NA
field_address$Address[grep("View Map", field_address$Address)] <- NA
field_address$Address <- gsub("^.+q=", "", field_address$Address)
field_address$Address <- gsub("&amp;ie.+$", "", field_address$Address)
field_address$Address <- gsub("%20", " ", field_address$Address)
field_address$Address <- gsub("&amp;", "&", field_address$Address)
field_address$Field <- gsub("[[:space:]]", " ", field_address$Field)
field_address$Address <- gsub("[[:space:]]", " ", field_address$Address)

field_address <- field_address %>% separate(Address, c("Street Address", "City", "State", "Zip Code"), "\\+")
field_address$City <- gsub(",$", "", field_address$City)
field_address$`Street Address` <- gsub(",$", "", field_address$`Street Address`)

## write.csv(field_address, "EDP_Fields_Addresses.csv", row.names = FALSE, na = "")