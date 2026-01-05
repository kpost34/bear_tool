# Run entire pipeline


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



# Data Acquisition==================================================================================
## Define target_name
target_name <- "ryegrass"

df_raw_data <- import_data(source="package", dataset_name=target_name, pkg="drc")



# Run Pipeline======================================================================================
## Step 1: Validate
my_suitcase <- validate_data(df_raw_data, dataset_name=target_name, target_dose_col="conc", 
                             target_response_col="rootl")


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




