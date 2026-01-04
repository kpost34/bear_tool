# Data import and validation checks for data


# Data Import=======================================================================================
import_data <- function(source, path, dataset_name, pkg){
  # Files
  if(source=="file"){
    ext <- tools::file_ext(path)
    if(ext=="csv"){
      raw_data <- read.csv(path)
    } else if(ext=="xlsx"){
      raw_data <- read_excel(path) %>%
        as.data.frame()
    }

    #store message
    import_msg <- "Success: Raw data imported from file."
    
  } else if(source=="package"){
    data(dataset_name, package=pkg, envir=environment())
    raw_data <- get(dataset_name)
    
    #store message
    import_msg <- "Success: Raw data imported from package."
  }
  
  # Print message & return data
  message(import_msg)
  return(raw_data)

}



# Check ID==========================================================================================
check_id <- function(df, dataset_name, id_col=NA){
  # If user-defined id_col absent
  if(!is.na(id_col) & !id_col %in% names(df)){
    #store obj
    id_status <- "Failed"
    id_msg <- paste(
      "Error: Specified id_col", 
      paste0("'", id_col, "'"),  
      "not found for dataset",
      paste0(dataset_name, ".")
    )
    #print msg
    warning(id_msg)
    #package obj
    suitcase=list(
      status=id_status,
      message=id_msg
    )
    #return
    return(suitcase)
      
    # If user-defined id_col present
    } else{
      if(!is.na(id_col)){
        #update df & store objs
        if("id" %in% names(df)){
          df <- rename(df, id_orig=id)
        }
        df <- rename(df, id:=!!sym(id_col)) %>%
          as_tibble()
        id_status <- "Success"
        id_msg <- ""
        id_label <- id_col
        
      # If id_col not user-defined
      } else{
          if("id" %in% names(df)){
            df <- rename(df, id_orig=id)
          }
          df <- rownames_to_column(df, var="id") %>%
            as_tibble()
          id_status <- "Success"
          id_msg <- "Note: rownames promoted to column 'id'"
          id_label <- "id"
          
          #print message
          message(id_msg)
      }
      #package objects
      suitcase=list(
        data=df,
        dataset_name=dataset_name,
        status=id_status,
        message=id_msg,
        id_label=id_label
      )
      
      #return objects
      return(suitcase)
    }
}



# Check Presence====================================================================================
check_presence <- function(suitcase, target_dose_col, target_response_col){
  df <- suitcase$data
  
  # Determine if inputs exist in df and how many are missing (if applicable)
  input_cols <- c(dose=target_dose_col, response=target_response_col)
  missing_user_names <- input_cols[!c(target_dose_col, target_response_col) %in% names(df)]
  
  # Create dynamic strings for what is missing
  n_cols_abs <- length(missing_user_names)
  str_cols_abs <- dynamic_paste(missing_user_names)
  
  
  # If one or both cols are missing
  if(n_cols_abs>0){
    #store objs
    pres_status <- "Failed"
    pres_msg <- paste("Error: ",
                      if(n_cols_abs==1){"Column"} else{"Columns"},
                      str_cols_abs,
                      "not found."
    )
    #print warning
    warning(pres_msg)
    
    #update and return suitcase
    suitcase$status <- pres_status
    suitcase$message <- append_message(suitcase$message, pres_msg)

    return(suitcase)
  
    # If both cols are present
  } else{
    #standardize names & store objs
    df <- df %>%
      rename(dose=!!sym(target_dose_col), response=!!sym(target_response_col))
    pres_status <- "Success"
    
    #update & return objs
    suitcase$data <- df
    suitcase$status <- pres_status
    suitcase$dose_label <- target_dose_col
    suitcase$response_label <- target_response_col
    
    return(suitcase)
  }
}



# Check Numeric=====================================================================================
check_numeric <- function(suitcase){
  df <- suitcase$data
  
  # Check if dose and response cols are numeric & combine status into vector
  dose_numeric <- is.numeric(df$dose)
  response_numeric <- is.numeric(df$response)
  
  numeric_cols_status <- c(dose=dose_numeric, response=response_numeric)
  
  nonnum_cols <- which(numeric_cols_status==FALSE) %>%
    names()
  
  # Create dynamic string for what is non-numeric
  n_cols_nonnum <- length(nonnum_cols)
  str_cols_nonnum <- dynamic_paste(nonnum_cols)
  
  # If one or both cols are non-numeric
  if(n_cols_nonnum > 0){
    #store objs
    num_status <- "Failed"
    num_msg <- paste("Error: ",
                     if(n_cols_nonnum==1){"Column"} else{"Columns"},
                     str_cols_nonnum,
                     if(n_cols_nonnum==1){"is"} else{"are"},
                     "non-numeric."
    )
    #print warning
    warning(num_msg)
    
    #update and return suitcase
    suitcase$status <- num_status
    suitcase$message <- append_message(suitcase$message, num_msg)

    return(suitcase)
    
    # If both cols are numeric
  } else{
    #return suitcase as is
    return(suitcase)
  }
}



# Check Missingness=================================================================================
check_missingness <- function(suitcase){
  df <- suitcase$data
  
  # Store objs
  n_total <- nrow(df)
  n_missing <- df %>%
    filter(is.na(dose)|is.na(response)) %>%
    nrow()
  
  pct_lost <- round((n_missing/n_total)*100, 1)
  
  # If excessive missingness
  if(pct_lost > 5){
    #store objs
    miss_status <- "Failed"
    miss_msg <- paste("Error:", 
                      paste0(pct_lost, "%"),
                      "of observations",
                      paste0("(", n_missing),
                      paste("rows) contain missing values. Threshold is 5%.")
    )
    
    #print warning
    warning(miss_msg)
    
    #update and return suitcase
    suitcase$status <- miss_status
    suitcase$message <- append_message(suitcase$message, miss_msg)
    
    return(suitcase)
    
    # If < 5% missingness
  } else{
    #drop NAs
    df <- df %>%
      drop_na(dose, response)
    
    miss_msg <- paste("Note: Removed", n_missing, "rows with NAs.")
    
    #update & return suitcase
    suitcase$message <- append_message(suitcase$message, miss_msg)
    
    return(suitcase)
  }
}



# Check Zero and Negative Values=====================================================================
## Subsubfunction
count_neg_zero_values <- function(df, col, value){
  n <- if(value=="neg"){
    df %>%
      filter({{col}} < 0) %>%
      nrow()
  } else if(value=="zero"){
    df %>%
      filter({{col}}==0) %>%
      nrow()
  }
  return(n)
}


## Subfunction
check_values <- function(suitcase){
  df <- suitcase$data
  
  # Count negative & 0 doses and responses
  n_neg_doses <- count_neg_zero_values(df, dose, "neg")
  n_zero_doses <- count_neg_zero_values(df, dose, "zero")
  n_neg_responses <- count_neg_zero_values(df, response, "neg")
  n_zero_responses <- count_neg_zero_values(df, response, "zero")
  
  # Negative doses
  if(n_neg_doses>0){
    #store objs
    values_status <- "Failed"
    neg_doses_msg <- paste("Error:", n_neg_doses, "doses detected.")
    
    #print warning
    warning(neg_doses_msg)
    
    #update & return suitcase
    suitcase$status <- values_status
    suitcase$message <- append_message(suitcase$message, neg_doses_msg)

    return(suitcase)
  }
  else{
    # No control group
    if(n_zero_doses==0){
      #store msg
      zero_doses_msg <- "Warning: No control group (dose = 0) detected."
      
      #print warning
      warning(zero_doses_msg)
      
      #update suitcase
      suitcase$message <- append_message(suitcase$message, zero_doses_msg)

    }
    # Negative responses
    if(n_neg_responses>0){
      #store msg
      neg_responses_msg <- paste("Note:", 
                                 n_neg_responses, 
                                 "negative responses detected.")
      
      #update suitcase
      suitcase$message <- append_message(suitcase$message, neg_responses_msg)
    }
    # Zero responses
    if(n_zero_responses>0){
      #store msg
      zero_responses_msg <- paste("Note:",
                                  n_zero_responses,
                                  "zero responses detected.")
      
      #udpate suitcase
      suitcase$message <- append_message(suitcase$message, zero_responses_msg)
    }
    
    #return suitcase
    return(suitcase)
  }
  
}



# Check Minimum Observations========================================================================
check_min_obs <- function(suitcase){
  df <- suitcase$data
  
  # Get unique doses and total obs
  n_dose <- length(unique(df$dose))
  n_obs <- nrow(df)
  
  # Insufficient doses
  if(n_dose<5){
    #store obs
    min_dose_status <- "Failed"
    min_dose_msg <- paste("Error: Only", n_dose, "doses detected. At least 5 required.")
    
    #print warning
    warning(min_dose_msg)
    
    #update suitcase
    suitcase$status <- min_dose_status
    suitcase$message <- append_message(suitcase$message, min_dose_msg)
  }
    
  # Insufficient observations
  if(n_obs<15){
    #store obs
    min_obs_msg <- paste("Warning: Only", n_obs, "observations detected. At least 15 required.")
    
    #print warning
    warning(min_obs_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, min_obs_msg)
  }
  
  #return suitcase
  return(suitcase)
  
}



# Validate Data (function)==========================================================================
validate_data <- function(df, dataset_name, target_id_col=NA, target_dose_col, target_response_col){
  # 1. Initialize Suitcase
  suitcase <- check_id(df, dataset_name, id_col=target_id_col)
  
  # 2. Presence Gate
  if(suitcase$status=="Success"){
    suitcase <- check_presence(suitcase, target_dose_col, target_response_col)
  }
  
  # 3. Numeric Gate
  if(suitcase$status=="Success"){
    suitcase <- check_numeric(suitcase)
  }
  
  # 4. Missingness Gate
  if(suitcase$status=="Success"){
    suitcase <- check_missingness(suitcase)
  }
  
  # 5. Value Audit (Negatives/Zeroes)
  if(suitcase$status=="Success"){
    suitcase <- check_values(suitcase)
  }
  
  # 6. Minimum Observation Gate
  if(suitcase$status=="Success"){
    suitcase <- check_min_obs(suitcase)
  }
  
  return(suitcase)
}



# #ARCHIVE============================================================================================
# # Check ID------------------------------------------------------------------------------------------
# #hard code
# ryegrass0 <- janitor::clean_names(ryegrass)
# 
# id_present <- "id" %in% (names(ryegrass0))
# 
# if(id_present){
#   msg_id_present <- "'id' present in column names"
# } else{
#   msg_id_present <- "'id' absent from column names. rownames promoted to column"
# }
# 
# df_ryegrass <- {if(!id_present) rownames_to_column(ryegrass0, var="id") else .} %>%
#   as_tibble()
# 
# 
# 
# # Column Presence-----------------------------------------------------------------------------------
# #columns
# col_dose <- "conc"
# col_response <- "rootl"
# col_id <- "id"
# 
# #columns as single-quoted strings
# str_dose <- paste0("'", col_dose, "'")
# str_response <- paste0("'", col_response, "'")
# 
# #vectors of unquoted and single-quoted columns
# vec_cols <- names(ryegrass)
# vec_cols_quote <- purrr::map_chr(vec_cols, function(x) paste0("'", x, "'"))
# n_cols <- length(vec_cols)
# 
# str_cols <- dynamic_paste(n=n_cols, vec=vec_cols_quote)
# 
# vec_cols_pres <- c(col_dose, col_response) %in% vec_cols
# names(vec_cols_pres) <- c("dose", "response")
# 
# vec_cols_abs <- which(vec_cols_pres==FALSE) %>%
#   names()
# 
# n_cols_abs <- length(vec_cols_abs)
# vec_cols_abs_quote <- purrr::map_chr(vec_cols_abs, function(x) paste0("'", x, "'"))
# 
# str_cols_abs <- dynamic_paste(n=n_cols_abs, vec=vec_cols_abs_quote)
# 
# #check for presence
# status_cols_pres <- n_cols_abs > 0
# 
# #check for mapping
# if(status_cols_pres){
#   msg_cols_pres <- paste("Validated: Modeling", 
#                          str_response,
#                          "as the response and",
#                          str_dose, 
#                          "as the dose. Proceed?"
#   )
# } else{
#   msg_cols_pres <- paste("Error: ",
#                          if(n_cols_abs==1){"Column"} else{"Columns"},
#                          str_cols_abs,
#                          if(n_cols_abs==1){"is"} else{"are"},
#                          "missing. Dataframe contains",
#                          paste0(str_cols, ".")
#   )
# }
# 
# 
# 
# # Check Numeric-------------------------------------------------------------------------------------
# vec_cols_class <- purrr::imap_chr(df_ryegrass[,c(col_dose, col_response)], 
#                                   ~class(.x))
# 
# vec_cols_nonnum <- which(vec_cols_class=="numeric") %>%
#   names()
# 
# n_cols_nonnum <- length(vec_cols_nonnum)
# vec_cols_nonnum_quote <- purrr::map_chr(vec_cols_nonnum, function(x) paste0("'", x, "'"))
# 
# status_cols_num <- n_cols_nonnum > 0
# 
# str_cols_nonnum <- dynamic_paste(n_cols_nonnum, vec_cols_nonnum_quote)
# 
# 
# if(status_cols_num){
#   msg_cols_num <- paste("Validated:",
#                         str_response, 
#                         "and", 
#                         str_dose,
#                         "are numeric. Proceed?"
#   )
# } else{
#   msg_cols_num <- paste("Error: ",
#                         if(n_cols_nonnum==1){"Column"} else{"Columns"},
#                         str_cols_nonnum,
#                         if(n_cols_nonnum==1){"is"} else{"are"},
#                         "non-numeric."
#   ) 
# }
# 
# 
# 
# # Check Missingness---------------------------------------------------------------------------------
# #Rename columns here:
# df_ryegrass1 <- df_ryegrass %>%
#   rename(dose=!!sym(col_dose), response=!!sym(col_response))
# 
# 
# #Identify and quantify missing values in the critical columns.
# 
# #hard-coded
# #1) count percentage of NAs in dose and response cols separately
# n_rows_orig <- nrow(df_ryegrass)
# 
# # vec_cols_na <- df_ryegrass[, c(col_dose, col_response)] %>%
# #   apply(2, function(x) sum(is.na(x))/n_rows_orig) 
# 
# vec_cols_na <- df_ryegrass1 %>%
#   select(dose, response) %>%
#   apply(2, function(x) sum(is.na(x))/n_rows_orig) 
# 
# 
# #2) if % >= 5%, return "Critical failure: Dose data incomplete."
# status_na_dose <- vec_cols_na["dose"] < 5
# # status_na_dose <- vec_cols_na["conc"] < 5
# 
# if(status_na_dose){
#   msg_na_dose <- "Critical failure: Dose data incomplete."
# }
# 
# 
# #3) if % >= 5%, return "Critical failure: Response data incomplete."
# status_na_response <- vec_cols_na["response"] < 5
# # status_na_response <- vec_cols_na["rootl"] < 5
# 
# if(status_na_response){
#   msg_na_response <- "Critical failure: Response data incomplete."
# }
# 
# 
# #4) if both pass, filter out rows containing any NAs and count n rows removed
# df_ryegrass_filt <- df_ryegrass1 %>%
#   filter(!is.na(dose), !is.na(response))
# 
# # df_ryegrass_filt <- df_ryegrass %>%
# #   filter(!is.na(!!sym(col_dose)), !is.na(!!sym(col_response)))
# 
# n_rows_filt <- nrow(df_ryegrass_filt)
# 
# n_rows_removed <- n_rows_orig - n_rows_filt
# 
# 
# #5) Return the cleaned dataframe AND the count of dropped rows as part of your Status Object
# status_na_dose
# status_na_response
# 
# msg_n_rows_removed <- paste("Validated:",
#                             n_rows_removed,
#                             "row(s) removed")
# 
# 
# 
# # Check Values--------------------------------------------------------------------------------------
# #1) Count of negative doses (should be 0; if >0, trigger a critical error)
# n_neg_doses <- count_neg_zero_values(df_ryegrass_filt, dose, "neg")
# 
# 
# if(n_neg_doses > 0){
#   msg_neg_doses <- "Critical error: Negative doses detected."
# }
# 
# 
# #2) Count of zero doses (should be >0; if 0, trigger a warning)
# n_zero_doses <- count_neg_zero_values(df_ryegrass_filt, dose, "zero")
# 
# if(n_zero_doses==0){
#   msg_zero_doses <- "Warning: No Control Group Detected"
# }
# 
# 
# #3) Count of negative responses
# n_neg_responses <- count_neg_zero_values(df_ryegrass_filt, response, "neg")
# 
# 
# 
# #4) Count of zero response
# n_zero_responses <- count_neg_zero_values(df_ryegrass_filt, response, "zero")
# 
# 
# 
# # Check Minimum Observations------------------------------------------------------------------------
# #1) Unique dose count
# n_dose <- df_ryegrass_filt %>%
#   summarize(n_dose=n_distinct(dose)) %>%
#   pull(n_dose) 
# 
# if(n_dose<5){
#   msg_n_dose <- "Critical error: Insufficient dose count. At least 5 unique doses required."
# }
# 
# #2) Total row count
# n_rows_filt #from earlier
# 
# if(n_rows_filt<15){
#   msg_n_rows <- "Critical error: Insufficent row count. At least 15 observations required."
# }







