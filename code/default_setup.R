# Default Setup for POA
library("tidyverse")
library("labelled")
library("rprojroot")

# update root file for every new survey
criterion <- rprojroot::is_rstudio_project
root <- find_root_file("fa23", criterion = criterion)

# code
source(file.path(root, "code/communities_speak_theme.R"))
source(file.path(root, "code/functions/make_plots.R"))

# data
codebook <- read_csv(file.path(root, "data/output/survey_codebook_labelled.csv"), show_col_types = FALSE)
load(file.path(root, "data/processed/survey_codebook_labeled.rdata"))
wrangled <- readRDS(file.path(root, "data/output/wrangled.rds"))
load(file.path(root, "data/processed/cleaned.rdata"))

# thresholds
# update thresholds according to new standards (including changing income brackets)
min_duration <- 100 # 100 seconds
min_completion <- .5 # responded to 50% of the questions
poverty_line <- 2 # poverty line is 36.5K (top of category 2)
poverty_line_val <- 36500
median_inc <- 3 # median income level is 69.5K (top of category 3)
median_inc_val <- 69500

# update demographics according to current convention
demographics <- c("gender", "race_census", "lgbtq", "below_median", "below_poverty", "borough", "senior_resp", "hh_elderly", "hh_child", "hh_disability", "english_lang", "religion_group")
names(demographics) <- demographics
