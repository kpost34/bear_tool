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
## Identify the study and mapping
target_name <- "ryegrass"


## Column Mapping (Raw names in your dataframe)
col_id       <- NA
col_dose     <- "conc"
col_response <- "rootl"


## Metadata (Human-readable labels for plots/tables)
meta_x_name  <- "Concentration"
meta_x_unit  <- "mM"
meta_y_name  <- "Root Length"
meta_y_unit  <- "mm"



# Data Acquisition==================================================================================
df_raw_data <- import_data(source="package", dataset_name=target_name, pkg="drc")



# Run Pipeline======================================================================================
## Step 1: Validate
my_suitcase <- validate_data(
  df_raw_data, 
  dataset_name=target_name, 
  target_id_col=col_id,
  target_dose_col=col_dose,
  target_response_col=col_response
  )


## Step 1.5: Inject Metadata
if(my_suitcase$status == "Success") {
  my_suitcase <- add_metadata(
    suitcase = my_suitcase,
    x_name   = meta_x_name, 
    x_unit   = meta_x_unit, 
    y_name   = meta_y_name, 
    y_unit   = meta_y_unit
  )
}


## Step 2: Audit
my_suitcase <- audit_suitcase(my_suitcase)


## Step 3: Model
my_suitcase <- select_best_model(my_suitcase)


## Step 4: Extract stats
my_suitcase <- extract_winning_stats(my_suitcase)


## Step 5: Predict
my_suitcase <- generate_prediction_data(my_suitcase)


## Step 6: Tabulate
my_suitcase <- get_summary_table(my_suitcase)




# Visual Generation=================================================================================
## Audit Visual
my_suitcase <- visualize_audit(my_suitcase)


## Final Curve Visual
my_suitcase <- visualize_results(my_suitcase)



# Export & Archive==================================================================================
## Image Export
export_plots(my_suitcase)


## Table Export
export_summary(my_suitcase)


## Model Export
save_model_object(my_suitcase)


## Full Archive
save_suitcase(my_suitcase)




