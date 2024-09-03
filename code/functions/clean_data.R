#' Clean survey data
#'
#' This function does not replace necssary manual validation steps.
#' 
#' Function cleans data by apply labels and values,
#' creating dummy variables for multiple answer questions.
#' ensuring variables are stored as the proper type,
#' changes binaries coded as 1,2 to 1,0, and
#' removes "prefer not to answer" from a multiple answer response.
#' 
#' df: the survey data.
#' col: unapplied argument intended to help problem-solving by specifying a single column
#' 
clean_data <- function(df = survey, col = NULL) {
  by_col <- lapply(setdiff(colnames(df), c("responseid", "recordeddate")), function(col) {
    #for (col in colnames(survey)[-1]) {
    index <- which(survey_codebook$q == col)
    values <- as.integer(unlist(stringr::str_split(survey_codebook$options[index], pattern = "[:punct:]")))
    tags <- unlist(str_split(survey_codebook$choices[index], "; "))
    
    recode_na <- any(str_detect(tags, "not applicable"))
    
    if(any(is.null(tags), is.na(recode_na))) {
      recode_na <- FALSE
    }
    
    if(all.equal(values, c(1,2)) == TRUE) {
      df[[col]] <- 2 - as.integer(df[[col]])
      values <- (2 - values)
      
      if(recode_na & all.equal(values, c(1,2)) == TRUE) {
        df[[col]][df[[col]] == -1] <- 2
        values[values == -1] <- 2
      }
    }
    
    named <- setNames(values, tags) %>% na.omit
    
    if(is.null(tags)) {
      named <- FALSE
    }
    
    
    if(col %in% c(likert, simple)) {
      out <- df[c("responseid", col)] %>%
        mutate_at(vars(col), ~labelled(as.integer(.), named))
      
      # multiple answer  
    } else if(col %in% mavr){
      
      sym_col <- sym(col)
      out <- df[c("responseid", col)] %>%
        fastDummies::dummy_cols(col, split = ",", ignore_na = TRUE) %>%
        tidytext::unnest_tokens(output = !!sym_col, token = "regex",
                                input = col, pattern = ",") %>%
        mutate_at(col, ~factor(as.integer(.), levels = values, labels = tags)) %>%
        group_by(responseid) %>% mutate_at(col, ~paste(., collapse = ";")) %>%
        distinct %>% na_if(NA)
      
      
      # don't double count prefer not to answer
      if(any(str_detect(tags, "prefer not to answer"))) {
        pnta <- which(names(named) == "prefer not to answer")
        pnta_col <- paste(c(col, pnta), collapse = "_")
        out <- out %>% mutate_at(pnta_col,
                                 funs(ifelse(str_detect(!!sym_col, "prefer not to answer;|;prefer not to answer"),
                                             0, !!sym(pnta_col)))) %>%
          mutate_at(col, ~str_replace(., "prefer not to answer;|;prefer not to answer", ""))
      }
      
      # haven label dummies
      cols_to_label <- out %>% ungroup %>% select_if(is.numeric) %>% colnames
      relabelled <- lapply(cols_to_label, function(dummy){
        i <- as.integer(str_replace(dummy, paste0(col, "_"), ""))
        values <- c(0, 1)
        names(values) <- c(glue::glue("not '{tags[i]}'"), tags[i])
        #names(values) <- c(paste("not", tags[i]), tags[i])
        out %>% ungroup %>% transmute_at(dummy, ~labelled(as.integer(.), values))
      }) %>% reduce(bind_cols)
      
      out[cols_to_label] <- relabelled
      
      #r_zaiyctblk6t1z33
    } else if(all(!str_detect(df[[col]], "[:alpha:]|^[:digit:]{3}(\\-|\\.)[:digit:]{3}(\\-|\\.)[:digit:]{4}$"), na.rm = TRUE) & any(!is.na(df[[col]]))) {
      out <- df[c("responseid", col)] %>% mutate_at(vars(col), as.integer)
    } else {
      out <- df[c("responseid", col)]  
    }
    
    #print(paste(col, recode_na, sep = ": "))
    #}    
    return(out)
  })
  
  by_col %>% reduce(full_join, by = c("responseid"))
  
}
