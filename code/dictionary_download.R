#### Downloading Survey Dictionary for Spring 2023 Communities Speak Individual Survey ####

# to be run after dictionary_upload

# importing libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

#working drive is only referencing fall 2023 Baseline Survey
setwd("/Users/marisalenaycarter/Desktop/Food-Insecurity-NYC/code")
getwd()

# load file names and folders as necessary created in dictionary_upload
load("data/processed/survey_codebook_types.rdata")
load("data/processed/survey_codebook_toname.rdata")

# from the labeling doc in the Google Drive, reading in "dummies_named" and "qs_named"
  # because of qualtrics data, also filtering out rows with fall 2022, and removing any parenthesis from q
named_dummies <- id_labeling %>% 
  read_sheet(sheet = "dummies_named", na = c("")) %>%
  filter(!grepl("fall 2022", q)) %>%
  mutate(q = str_replace_all(q, "\\s*\\([^\\)]+\\)", ""))
named_qs <- id_labeling %>% 
  read_sheet(sheet = "qs_named", na = c("")) %>%
  filter(!grepl("fall 2022", q)) %>%
  mutate(q = str_replace_all(q, "\\s*\\([^\\)]+\\)", ""))

# creating survey_codebook_labelled, that now included the nice names for vars, e.g. "q3" becomes "zip"
survey_codebook_labelled <- named_dummies %>%
  # joining named_dummies with to_name, named_qs
  left_join(to_name, by = c("qid", "q", "to_name")) %>%
  left_join(named_qs %>% rename(q_stem = q) %>% select(q_stem, name), by = "q_stem") %>%
  
  # binding named_qs (only the rows with qs that are not in named_dummies) with survey_codebook
  bind_rows(named_qs %>% filter(!q %in% named_dummies$q) %>%
              left_join(survey_codebook, by = c("qid", "q", "block_title", "text", "part"))) %>%
  
  # creating full name for dummies
  mutate(full_name = ifelse(is.na(sub_name), name, glue::glue("{name}_{sub_name}"))) %>%
 
   # taking only the variables in select
  select(qid, q, full_name, type, selector, subselector, text, part, to_name, options, choices, block_title, question) %>%
  
  # defining question as the number extracted from q
  # defining origin depending on if came from named_dummies or not
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         origin = ifelse(q %in% named_dummies$q, "survey question dummy", "survey question")) %>%
  
  # orders the rows by question number
  arrange(question)

# writing survey_codebook_labelled to csv file
write_excel_csv(survey_codebook_labelled, file = "data/output/survey_codebook_labelled.csv")

# grabbing new_vars info from the labeling doc
new_vars <- id_labeling %>% read_sheet(sheet = "new_vars")

# saving as rdata that can be loaded
save(survey_codebook_labelled, new_vars, file = "data/processed/survey_codebook_labeled.rdata")

# now the labeled codebook has been created! onto the cleaning code
