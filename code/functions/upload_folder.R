#' Upload a folder to a google olders have the same
#' internal structure.drive folder
#'
#' This will iterate through a list of documents in a specified folder
#' to update them to a specified google drive file path.
#' NOTE: this function assumes the git repository and google f
#' 
#' googledrive_path: folder to upload to.  defaults to sp22 currently.
#' project_path: filepath of the root folder (e.g. ~communities_speak/sp22)
#' today: today's date, which is automatically appended to the name of the file upon upload
#' pattern.: optional argument to specify the file pattern of the documents to upload
#' file_path: the folder of the documents to be uploaded
upload_folder <- function(googledrive_path = "Communities Speak/Subteams/Data Subteam/cleaning/sp22",
                          project_path = rprojroot::find_root_file("fa22",
                                                                   criterion = rprojroot::has_file(".git/index")),
                          today = gsub("-", "", Sys.Date()),
                          pattern. = NULL,
                          file_path)
  {
  
  # error handling
  if(str_sub(file_path, -1) == "/") {
    message("File path must point to a folder without slash '/'")
    stop(str_sub(file_path, -1) == "/")
  }
  
  files <- list.files(glue::glue("{project_path}/{file_path}"), full.names = TRUE)
  
  if(!is.null(pattern.)) {
    files <- grep(files, pattern = pattern., value = TRUE)
  }
  
  lapply(files, function(file) {
    split <- unlist(stringr::str_split(file, pattern = "\\.|\\/"))
    name <- str_replace_all(dplyr::nth(split, -2),
                            c("survey_codebook_" = ""))
    ending <- dplyr::last(split)
    modified <- stringr::str_replace_all(as.Date(file.info(file)$mtime), "-", "")
    
    spreadsheet <- NULL
    if(ending == "csv") {
      spreadsheet <- "spreadsheet"
    }
    
    if(today > modified) {
      message(glue::glue("{name} last modified on {modified}."))
      return(NULL)
    } else {
      drive_upload(media = file,
                   path = glue::glue("{googledrive_path}/{file_path}/{name}{today}.{ending}"),
                   type = spreadsheet)
    }
    
  })
  
}
