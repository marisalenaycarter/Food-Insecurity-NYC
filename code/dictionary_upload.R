#first step 


# importing libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(rjson)

setwd("/Users/marisalenaycarter/Desktop/Food-Insecurity-NYC/code")

# connecting to Google Drive
gs4_found <- gs4_find()

#### this section would be for adding in labels in advance

# finding ID for the Google Sheets file in the Drive -- should create empty file first
id_labeling <- gs4_found %>% filter(name == "labeling_fa23") %>% pull(id)

# using .qsf file downloaded from Qualtrics for specific survey, extracting relevant elements
file <- fromJSON(file = "data/input/F23_Baseline_Survey.qsf")$SurveyElements
elements <- unlist(lapply(file, function(element) element$PrimaryAttribute))

#### record survey question blocks

# matching each question (qid) to block title, e.g. "socio-economics"
blocks <- lapply(file[[1]]$Payload, function(block) {
  # getting description of block
  block_title = block$Description
  # getting qid for questions, ignoring if not a question
  qid <- lapply(block$BlockElements, function(element){
    type = element$Type
    if(type == "Question") {
      qid <- element$QuestionID
    } else {
      qid <- NULL
    }
    return(qid)
  }) %>% unlist
  # creating tibble matching block_title with qid
  return(tibble(block_title, qid))
}) %>% bind_rows() %>% mutate(across(everything(), stringr::str_to_lower))

#### pull out question information

# creating the survey codebook
survey_codebook <- lapply(file[str_detect(elements, "QID")], function(element) {
  # defining relevant variables needed for codebook
  qid = element$PrimaryAttribute
  q = element$Payload$DataExportTag 
  type = element$Payload$QuestionType
  selector = element$Payload$Selector
  subselector = element$Payload$SubSelector
  text = stringr::str_replace_all(element$Payload$QuestionText, c("<.*?>" = "", "&nbsp;" = " "))
  
  # contains the choice options, e.g. 1 "Male", 2 "Female", etc.
  unlisted = unlist(lapply(element$Payload$Choices, function(element) trimws(element$Display)))
  
  # for matrix, need to update unlisted because there are options for both rows and columns
  # unlisted becomes the column options, e.g. 1 "Excellent", 2 "Good", etc.
  # part becomes the rows
  if(type == "Matrix"){
    part = unlisted
    unlisted = unlist(lapply(element$Payload$Answers, function(element) trimws(element$Display)))
  } else {
    part = NA_character_
  }
  
  # adjusting names / order if there is RecodeValues
  if("RecodeValues" %in% names(element$Payload)) {
    if(qid=="QID35") {
      names(unlisted) <- 1:length(unlisted)
    } else {
      names(unlisted) <- element$Payload$RecodeValues
      unlisted <- unlisted[order(as.integer(names(unlisted)))]
    }
  } 
  
  # adjusting names / order if there is ChoiceDataExportTags
  if(type == "Matrix" & "ChoiceDataExportTags" %in% names(element$Payload) &
     typeof(element$Payload$ChoiceDataExportTags) == "list") {
    # adjusting names / order if there is RecodeValues
    q_pre <- paste0(q, "_")
    names(part) <- lapply(element$Payload$ChoiceDataExportTags,
                   function(element) gsub(q_pre, "", element)) %>% unlist()
    
  } 
  
  # concatenating all choices, separated by ';'
  choices = paste(unlisted, collapse = "; ")
  
  # handling free form entry (no options given, respondent must input themselves), e.g. zip code
  if(selector == "FORM") {
    part = unlisted
    choices = "free form entry"
    options = NA_character_
  }
  
  # options for the numeric options, e.g. 1,2,3
  if(length(unlisted) > 0){
    options = paste(as.integer(names(unlisted)), collapse = ",")
  } else {
    options = NA_character_
    choices = NA_character_
  }
  
  # text entry items
  text_entry = unlist(sapply(element$Payload$Choices, function(choice) choice$TextEntry))
  
  # adding index info and "text" to title for text entry questions, e.g. Q9 becomes Q9_6_text
  if(all(text_entry == "true", !is.null(text_entry))) {
    # adjusting names / order if there is RecodeValues
    if("RecodeValues" %in% names(element$Payload)) {
      translation <- element$Payload$RecodeValues
      index <- which(names(translation) == names(text_entry))
      names(text_entry) <- translation[index]
    }
    
    # adding the index and text
    post = glue::glue("_{names(text_entry)}_text")
    q = paste0(q, c("", post))
  } 
  
  # fixing q33 (qid35) because it was skipping 4
  # if(qid == "QID35") names(part) <- 1:length(part)
  
  # creating the output
  out <- tibble(qid, q, type, selector, subselector, text,
                part,
                options, choices)
  
  # dealt with when text_entry is "true", now when it's not
  if((nrow(out) > 1 & any(text_entry != "true", is.null(text_entry))) | type == "Matrix"){
    # for matrix questions, gluing the part responses to be part of the q name
    out <- out %>%
      mutate(temp = ifelse(type == "Matrix", names(part), names(unlisted)),
             q = glue::glue("{q}_{temp}")) %>% 
      select(-temp) # removing temp
  }
  
  return(out)
}) %>% 
  bind_rows() %>% 
  mutate_all(str_to_lower) %>% 
  filter(q != "") %>% # removing empty questions
  # defining question as the number extracted from q
  mutate(question = as.integer(str_extract(q, "[:digit:]{1,2}")),
         # when a var has text, making choices and options NA
         choices = ifelse(str_detect(q, "text"), NA, choices),
         options = ifelse(str_detect(q, "text"), NA, options)) %>%
  arrange(question) %>% 
  left_join(blocks) %>% 
  filter(!str_detect(block_title, "trash")) %>% # removing trash blocks
  filter(!grepl("fall 2022", q)) %>% # removing fall columns
  mutate(q = str_replace_all(q, "\\s*\\([^\\)]+\\)", "")) %>% # removing everything in parentheses
  # mutate(q = ifelse(qid == "qid111", "q_new_disability", q)) %>% # changing name for disability q to be easy to work with
  mutate(q = str_replace_all(q, " ", "")) # removing all spaces from q

#### saving info from survey_codebook

# creating vectors of the q's for each category, as defined 
  # simple -- multiple choice, select one
  # text -- free form text responses
  # likert -- "how would you rate..."
  # mavr -- multiple choice, select all that apply, no text
  # ro -- rank order, e.g. which mode of transportation do you most frequently use
simple <- survey_codebook %>% filter(type == "mc", selector == "savr", !str_detect(q, "text")) %>% pull(q)
text <- survey_codebook %>% filter(type == "te" | str_detect(q, "text")) %>% pull(q) 
likert <- survey_codebook %>% filter(selector == "likert", subselector != "multipleanswer") %>% pull(q)
mavr <- survey_codebook %>% filter(type == "mc" & selector == "mavr" | subselector == "multipleanswer", !str_detect(q, "text")) %>% pull(q)
ro <- survey_codebook %>% filter(type == "ro") %>% pull(q)

# saving as rdata so the vars can be referenced by loading the file
save(survey_codebook, simple, text, likert, mavr, ro, file = "data/processed/survey_codebook_types.rdata")

# the q's that need to be named
to_name <-
  # filter for variables to be dummied, taking both mavr and simple
  survey_codebook %>% filter(q %in% mavr | q %in% ro) %>%
  # list them out in long format
  tidytext::unnest_tokens(output = to_name, token = "regex", input = choices, pattern = ";", drop = FALSE) %>%
  group_by(q) %>%
  # number them according to their coding in qualtrics, but leave behind the q_stem for later merging
  mutate(q_stem = q, q = glue::glue("{q}_{row_number()}")) %>%
  mutate(to_name = trimws(to_name))

# label question stems
qs_to_name <- survey_codebook %>% select(qid, q, block_title, text, part)
# label dummy variable labels
dummies_to_name <- to_name %>% ungroup %>% select(qid, q, to_name) %>% unique %>% filter(!is.na(to_name))

# saving as rdata
save(to_name, qs_to_name, dummies_to_name, id_labeling, file = "data/processed/survey_codebook_toname.rdata")

# creating new sheets in the labeling doc in the Google Drive
write_sheet(qs_to_name, id_labeling, "qs_to_name")
write_sheet(dummies_to_name, id_labeling, "dummies_to_name")

# next step is to hand write in the new variable names for each q in the labeling doc
# should create two new sheets, "qs_named" and "dummies_named"
