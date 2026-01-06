# Initialize filesystem and export plots, summary table, model, and suitcase


# Set Up Function===================================================================================
initialize_filesystem <- function(){
  # Create folder dirs
  fp_plot <- here("output", "plots")
  fp_table <- here("output", "tables")
  fp_model <- here("output", "models")
  
  # Create folders if they do not exist
  if(!dir.exists(fp_plot)){
    dir.create(fp_plot, recursive=TRUE)
  }
  
  if(!dir.exists(fp_table)){
    dir.create(fp_table, recursive=TRUE)
  }
  
  if(!dir.exists(fp_model)){
    dir.create(fp_model, recursive=TRUE)
  }
  
  # Print message
  message("File system initialized: Output directories verified.")
}


# Image Export======================================================================================
export_plots <- function(suitcase){
  # Extract suitcase objs
  name <- suitcase$dataset_name
  plot_audit <- suitcase$audit_plot
  plot_final <- suitcase$modeling$plot
  
  # Create fp and filenames
  fp_plot <- here("output", "plots")
  
  nm_audit <- "audit_plot"
  nm_final <- "final_curve"
  
  fp_audit_png <- here(fp_plot, paste0(name, "_", nm_audit, ".png"))
  fp_audit_pdf <- here(fp_plot, paste0(name, "_", nm_audit, ".pdf"))
  
  fp_final_png <- here(fp_plot, paste0(name, "_", nm_final, ".png"))
  fp_final_pdf <- here(fp_plot, paste0(name, "_", nm_final, ".pdf"))
  
  # Save plots
  #audit-png
  ggsave(filename=fp_audit_png,
         plot=plot_audit,
         width=7,
         height=5,
         units="in",
         dpi=300)
  
  #audit-pdf
  ggsave(filename=fp_audit_pdf,
         plot=plot_audit,
         width=7,
         height=5,
         units="in",
         dpi=300)
  
  #final-png
  ggsave(filename=fp_final_png,
         plot=plot_final,
         width=7,
         height=5,
         units="in",
         dpi=300)
  
  #final-pdf
  ggsave(filename=fp_final_pdf,
         plot=plot_final,
         width=7,
         height=5,
         units="in",
         dpi=300)
  
  # Print message
  plots_msg <- paste("Success: 4 plot files (PNG/PDF) exported for", 
                     name,
                     "to output/plots/")
  message(plots_msg)
}



# Export Summary Table==============================================================================
export_summary <- function(suitcase){
  # Extract suitcase info
  name <- suitcase$dataset_name
  df_results0 <- suitcase$modeling$results
  
  # Clean up results data
  df_results <- df_results0 %>%
    mutate(across(where(is.numeric), ~round(.x, 4))) %>%
    set_names(
      c("ED50 Estimate", "ED50 Std. Error", "ED50 95% CI (Lower)", "ED50 95% CI (Upper)", 
        "Slope (b)", "Lower Limit (c)", "Upper Limit (d)", "Asymmetry (f)", "AIC", 
        "Pseudo R-Squared", "Slope P-Value", "Residual Std. Error", "Model Type", "Fit Status", 
        "Selection Rationale")
    )
  
  # Save to csv
  fp_table <- here("output", "tables", paste0(name, "_summary_stats.csv"))
  write_csv(x=df_results, file=fp_table)
    
  # Print message
  table_msg <- paste("Success: Summary table exported for",
                     name,
                     "to output/tables/")
  message(table_msg)
}



# Save Model========================================================================================
save_model_object <- function(suitcase){
  # Extract objs from suitcase
  name <- suitcase$dataset_name
  mod <- suitcase$modeling$winner
  
  # Save to rds
  fp_model <- here("output", "models", paste0(name, "_model.rds"))
  saveRDS(object=mod, file=fp_model)
  
  # Print message
  model_msg <- paste("Success: Model object for",
                     name,
                     "saved to output/models/")
  message(model_msg)
}



# Save Suitcase=====================================================================================
save_suitcase <- function(suitcase){
  # Extract obj from suitcase
  name <- suitcase$dataset_name
  
  # Define path
  fp_models <- here("output", "models")
  fp_suitcase <- here(fp_models, paste0(name, "_full_suitcase.rds"))
  
  # Save suitcase
  saveRDS(object=suitcase, file=fp_suitcase)
  
  # Print message
  message("Analysis state has been archived.")
                      
}



# ARCHIVE===========================================================================================
# Image Export--------------------------------------------------------------------------------------
# fname_plot_png <- "plot_ryegrass_rootlength.png"
# fp_out_plot_png <- here("output", "plots", fname_plot_png)
# 
# ggsave(filename=fp_out_plot,
#        plot=plot_draft,
#        width=6,
#        height=4,
#        units="in",
#        dpi=300)
# 
# 
# fname_plot_pdf <- "plot_ryegrass_rootlength.pdf"
# fp_out_plot_pdf <- here("output", "plots", fname_plot_pdf)
# 
# 
# ggsave(filename=fp_out_plot_pdf,
#        plot=plot_draft,
#        width=6,
#        height=4,
#        units="in",
#        dpi=300)
# 
# 
# 
# # Export Summary------------------------------------------------------------------------------------
# fname_table <- "table_ryegrass_rootlength.png"
# fp_out_table <- here("output", "tables", fname_table)
# 
# write_csv(df_final_summary, fp_out_table) #after creating df_final_summary
# 
# 
# 
# # Save Model----------------------------------------------------------------------------------------
# fname_model <- "model_ryegrass_rootlength.rds"
# fp_out_model <- here("output", "models", fname_model)
# 
# saveRDS(mod_final, fp_out_model)









