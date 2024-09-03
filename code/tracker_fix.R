library(googledrive)
library(googlesheets4)
library(tidyverse)

source("code/default_setup.R")

tracker_id <- gs4_find() %>% filter(name == "tracker_fa22") %>% pull(id)

# fix emerson tracking field
write_sheet(final_clean %>% filter(is.na(test_response)) %>% select(responseid, aid),
            tracker_id, "eme_fix")

# fix q language after adding official embedded data field in later
#write_sheet(final_clean %>% filter(is.na(test_response)) %>% select(userlanguage),
#            borough_id, "language_fix")