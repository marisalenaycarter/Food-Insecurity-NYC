library(googledrive)
library(googlesheets4)
library(tidyverse)

setwd("~/communities_speak/fa22/data/input")
boroughs <- sf::st_read("zipcodes/ZIP_CODE_040114.shp") %>%
  mutate(borough = case_when(
    COUNTY == "New York" ~ "Manhattan",
    COUNTY %in% c("Bronx", "Queens") ~ COUNTY,
    PO_NAME %in% c("Brooklyn", "Staten Island") ~ PO_NAME)) %>%
  mutate(borough = str_to_lower(borough))

boroughs_csv <- boroughs %>%
  as_tibble() %>% transmute(zip = as.integer(ZIPCODE), borough) %>% arrange(borough, zip) %>% na.omit() %>% distinct %>%
    filter(!(zip == 11370 & borough == "bronx"))

#borough_list <- c("bronx", "brooklyn", "manhattan", "queens", "staten island")
#borough_list <- setNames(1:5, borough_list)


#url <- "https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pone.0194799/1/pone.0194799.s001.pdf?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20220414%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220414T142942Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=8590210be75efe43086a7c2ec2b14d4a3c3c673a3609c0695f201403b20893e7ff5f7cc2047629fd7dc866340a409e9955b05c6d8d327e83b8d23f2041c9f5f7d3be1d34b10671ad8f695b1b7455309683482a45ff52bdf46390b24dd4fc47509601a3dba0a201a273161b41424e42fc21478d4284155a946c708f7dff9b30834b149343115fe4d2b7fc6ff8c4ce85c3ba6f735767be2bf45bb9aa56f8d52cd7dc391fd80f7f5519794f9f8a3056c755c75a26f8838eff0efe74f9a1ea65eedfb5ff50f2a0d9631e419979cb85511555a3d1c5b61e65ee49d3f4782eb8d413684580b57e4a84186f738fe89c22676dcc1f54d205f587d7a7426e4856abb9e3d1"
#pdf <- pdftools::pdf_data(url) %>% as.data.frame

#bronx_y <- 102
#brooklyn_y <- 205
#manhattan_y <- 361
#queens_y <- 503
#staten_y <- 646



#boroughs <- pdf %>% filter(str_detect(text, "[:digit:]")) %>% tidytext::unnest_regex(text, text, ",") %>%
#  mutate(borough = labelled(case_when(
#    y > staten_y ~ 5,
#    y > queens_y ~ 4,
#    y > manhattan_y ~ 3,
#    y > brooklyn_y ~ 2,
#    y > bronx_y ~ 1), borough_list)) %>% as_tibble %>%
#  mutate(zip = as.integer(text)) %>%
#  select(zip, borough)

#table <- read_html("https://www.nycbynatives.com/nyc_info/new_york_city_zip_codes.php#:~:text=New%20York%20City%20Zip%20Codes%20%20%2010001,%20%2010455%20%2063%20more%20rows%20?msclkid=cf2f6842bc2311ec8f2a71b6a935d6b3") %>%
#  html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]]


#read_html("https://www.zipdatamaps.com/list-of-zip-codes-in-new-york.php?msclkid=13e6ea2bbc2711ecaadb1b5df108914f") %>%
#  html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
#  mutate(borough = case_when(
#    str_detect(X4, "^Queens$") ~ X4,
#    str_detect(X3, "^(Brooklyn|Bronx|Staten Island)$") ~ X3
#  )) %>%  view
## /html/body/div[1]/div[2]/div/table


save(boroughs_csv, file = "boroughs.rdata")
write_csv(boroughs_csv, file = "boroughs.csv")

borough_id <- gs4_find() %>% filter(name == "tracker_sp22") %>% pull(id)
write_sheet(boroughs_csv, borough_id, "zip_dictionary")
