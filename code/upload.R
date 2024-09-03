# upload
library(googledrive)
library(googlesheets4)

# update for each new survey and in functions/upload_folder.R
criterion <- rprojroot::has_file(".git/index")
root <- find_root_file("fa22", criterion = criterion)
source(file.path(root, "code/functions/upload_folder.R"))

upload_folder(file_path = "data/processed")
upload_folder(file_path = "data/output")
upload_folder(file_path = "reports/poa", pattern. = "pdf")
upload_folder(file_path = "reports/data_exploration", pattern. = "pdf")

today <- gsub("-", "", Sys.Date())
googledrive_path <- "Communities Speak/Subteams/Data Subteam/cleaning/sp22/"
project_path <- "~/communities_speak/sp22/"

# upload code/functions
lapply(list.files(glue::glue("{project_path}code/functions"), full.names = TRUE), function(file){
  partial <- last(unlist(str_split(file, pattern = "/")))
  drive_upload(media = file, path = glue::glue("{googledrive_path}code/functions/{partial}"), overwrite = TRUE) 
})

# upload specific files
drive_upload(media = glue::glue("{project_path}code/cleaning.Rmd"),
             path = glue::glue("{googledrive_path}code/cleaning{today}.Rmd"), overwrite = TRUE)
drive_upload(media = glue::glue("{project_path}reports/emerson_demographics.pdf"), path = glue::glue("{googledrive_path}reports/panel_vs_online{today}.pdf"), overwrite = TRUE)

