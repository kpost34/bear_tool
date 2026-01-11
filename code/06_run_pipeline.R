# Run Entire Pipeline


# Load Packages, Source Functions, & Initialize Filesystem==========================================
## Packages
library(pacman)
pacman::p_load(
  here,
  tidyverse,
  drc,
  janitor,
  DT
)


## Functions
fp_code <- here("code")
scripts <- list.files(path=fp_code, pattern="0[0-5]", full.names=TRUE) 
purrr::walk(scripts, source)


## Filesystem
initialize_filesystem()



# User Configuration================================================================================
## Identify the study, path (if nec), and mapping
target_name <- "chaos_data"
fp_data <- here("data", paste0(target_name, ".csv"))


## Mapping: Define which columns in your raw data map to the analysis
col_id       <- NA
col_dose     <- "conc"
col_response <- "resp"


## Metadata: Human-readable labels for plots and tables
meta_x_name  <- "Dose"
meta_x_unit  <- "mg/L"
meta_y_name  <- "Growth"
meta_y_unit  <- "mg"



# Data Acquisition==================================================================================
# df_raw_data <- import_data(source="package", dataset_name=target_name, pkg="drc")
df_raw_data <- import_data(source="file", path=fp_data)


# Run Pipeline======================================================================================
## GATE 1: Structural Validation
my_suitcase <- validate_data(
  df_raw_data, 
  dataset_name=target_name, 
  target_id_col=col_id,
  target_dose_col=col_dose,
  target_response_col=col_response
  )

if(my_suitcase$status == "Success") {
  
  ### Inject labels for plots/tables
  my_suitcase <- add_metadata(
    suitcase = my_suitcase,
    x_name   = meta_x_name, 
    x_unit   = meta_x_unit, 
    y_name   = meta_y_name, 
    y_unit   = meta_y_unit
  )

  
  ## GATE 2: Biological Audit
  ### Calculates stats and checks for monotonicity/SNR
  my_suitcase <- audit_suitcase(my_suitcase)
  
  ### Always generate diagnostic audit plot if correct columns present
  my_suitcase <- visualize_audit(my_suitcase)
  
  
  ## GATE 3: Potency Modeling 
  ### Only attempt curve fitting if the data is biologically sound (Status is still "Success")
  if(my_suitcase$status == "Success") {
    
    my_suitcase <- my_suitcase %>%
      select_best_model() %>%
      extract_winning_stats() %>%
      generate_prediction_data() %>%
      get_summary_table() %>%
      visualize_results()
    
      message(">>> Success: Full analysis completed for ", target_name)
    
  } else {
    message(">>> Warning: Biological Audit Failed: Results limited to diagnostics.")
  }
  
} else {
  message(">>> Error: Structural Validation failed. Pipeline halted.")
}



# Export and Archive===============================================================================
## These functions use internal safety guards to only save what exists
my_suitcase <- my_suitcase %>%
  export_plots() %>%
  export_summary() %>%
  save_model_object() %>%
  save_suitcase()

message(">>> Archive Complete: Analysis state saved to output/models/")


