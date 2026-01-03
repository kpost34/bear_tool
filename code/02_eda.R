# EDA of drc ryegrass dataset


# Calculate Dose Stats==============================================================================
calc_dose_stats <- function(suitcase){
  df <- suitcase$data
  
  # Calculate summary stats
  df1 <- df %>%
    group_by(dose) %>%
    summarize(
      n=n(),
      mean_response=mean(response),
      sd_response=sd(response),
      cv_response=sd_response/mean_response,
      q1_response=quantile(response, 0.25),
      q3_response=quantile(response, 0.75),
      iqr_response=q3_response-q1_response
    ) %>%
    ungroup() %>%
    mutate(cv_response_median=median(cv_response))
  
  # Update & return suitcase
  suitcase$stats <- df1
  
  return(suitcase)
  
}



# Audit Variance====================================================================================
audit_variance <- function(suitcase){
  df <- suitcase$stats
  suitcase$high_cv_doses <- NULL
  
  # Compare individual to global CV
  dose_cv_high <- df %>%
    mutate(
      #add tiny floor in case any median is 0
      cv_response_median=cv_response_median + 0.001,
      cv_high=cv_response > (cv_response_median * 2)) %>%
    filter(cv_high) %>%
    pull(dose)
  
  n_cv_high <- length(dose_cv_high)
  
  str_cv_high <- dynamic_paste(dose_cv_high)
  
  # Pull noisy doses
  if(n_cv_high > 0){
    #store message
    var_msg <- paste("Warning: High replicate variance detected at",
                     if(n_cv_high==1){"Dose"} else{"Doses"},
                     paste0(str_cv_high, ".")
    )
    
    #print warning
    warning(var_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, var_msg)
    suitcase$high_cv_doses <- dose_cv_high
  } 
  
  # Return suitcase
  return(suitcase)
}



# Audit Distribution of Doses=======================================================================
check_dose_range <- function(suitcase){
  df <- suitcase$data
  
  # Store objs
  max_dose <- max(df$dose)
  
  min_dose <- df %>%
    filter(dose!=0) %>%
    pull(dose) %>%
    min()
  
  dose_range <- max_dose/min_dose
  suitcase$dose_ratio <- dose_range
  
  # Small dose range
  if(dose_range<10){
    #store
    dose_range_msg <- "Warning: Limited dose range detected. Curve estimation may be unstable."
    
    #print warning
    warning(dose_range_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, dose_range_msg)
  
  }
  
  # Return suitcae
  return(suitcase)
}



# Detect Outliers===================================================================================
detect_outliers <- function(suitcase){
  suitcase$outlier_indices <- NULL
  
  # Join data & stats table and extract outlier indices & counts
  df_data <- suitcase$data
  df_stats <- suitcase$stats
  
  df_outliers <- df_data %>%
    left_join(df_stats, by="dose") %>%
    mutate(
      thresh_min=q1_response-(1.5*iqr_response),
      thresh_max=q3_response+(1.5*iqr_response),
      outlier=response < thresh_min|response > thresh_max
    ) %>%
    filter(outlier) %>%
    distinct(id, dose)
  
  outlier_indices <- pull(df_outliers, id)
  n_outliers <- length(outlier_indices)
  
  outlier_dose_counts <- df_outliers %>%
    group_by(dose) %>%
    summarize(n=n()) %>%
    ungroup() %>%
    mutate(n_per_dose = paste0(dose, "(", n, ")")) %>%
    pull(n_per_dose)
  
  str_outlier <- dynamic_paste(outlier_dose_counts, add_quotes=FALSE)
  
  # Outliers found
  if(n_outliers > 0){
    #store message
    outlier_msg <- paste("Warning: Outliers detected at the following doses (n):",
                         paste0(str_outlier, "."),
                         "Model was fit using all data."
    )
    
    #print warning
    warning(outlier_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, outlier_msg)
    suitcase$outlier_indices <- outlier_indices
  }
  
  # Return suitcase
  return(suitcase)
  
}



# Audit Monotonicity================================================================================
audit_monotonicity <- function(suitcase){
  df <- suitcase$stats
  
  # Determine monotonicity failure
  df_mono <- df %>%
    summarize(
      #response range
      span=max(mean_response) - min(mean_response),
      #net change
      delta=mean_response[dose==min(dose)]-mean_response[dose==max(dose)],
      direction=case_when(
        delta > 0 ~ "Inhibition",
        delta < 0 ~ "Activation",
        delta==0  ~ "None"
      ),
      mono_ratio=abs(delta)/span,
      mono_fail=mono_ratio<0.1
    )
  
  # Calculate pooled SNR
  snr <- df_mono$span/mean(df$sd_response)
  
  # Flat line
  if(df_mono$delta==0){
    #store objs
    mono_status <- "Failed"
    mono_msg <- "Error: No response detected. The mean response at the highest dose is identical
                to the control"
    
    #print warning
    warning(mono_msg)
    
    #update & return suitcase
    suitcase$status <- "Failed"
    suitcase$message <- append_message(suitcase$message, mono_msg)
    suitcase$direction <- df_mono$direction
    
    return(suitcase)
    
    # Non-monotonicity
  } else if(df_mono$mono_fail){
    #store objs
    mono_status <- "Failed"
    mono_msg <- "Error: Non-monotonic data detected. Net displacement is insufficient relative to 
                 total variance"
    
    #print warning
    warning(mono_msg)
    
    #update & return suitcase
    suitcase$status <- mono_status
    suitcase$message <- append_message(suitcase$message, mono_msg)
    suitcase$direction <- df_mono$direction
    
    return(suitcase)
    
    # Low SNR
  } else if(snr<3){
    #store message
    snr_msg <- "Warning: Low Signal-to-Noise Ratio (SNR < 3). Curve fit may be unreliable."
    
    #print warning
    warning(snr_msg)
    
    #update & return suitcase
    suitcase$direction <- df_mono$direction
    suitcase$snr <- snr
    suitcase$message <- append_message(suitcase$message, snr_msg)
    
    return(suitcase)
    
    # No flags
  } else{
    #update & return suitcase
    suitcase$direction <- df_mono$direction
    suitcase$snr <- snr
    
    return(suitcase)
  }

}



# Audit Plateau=====================================================================================
check_plateau <- function(suitcase){
  df <- suitcase$stats
  
  # Calculate span
  span <- max(df$mean_response) - min(df$mean_response)

  # Calculate terminal shift
  terminal_shift <- df %>%
    arrange(dose) %>%
    slice_tail(n=2) %>%
    pull(mean_response) %>%
    diff() %>%
    abs()
  
  # Compare span and terminal shift
  shift_large <- terminal_shift > (0.15*span)
  
  # Large terminal shift
  if(shift_large){
    #store message
    plateau_msg <- "Warning: Top plateau not reached. EC50 estimation may be extrapolated."
    
    #print warning
    warning(plateau_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, plateau_msg)
  }
  
  # Update & return suitcase
  suitcase$at_plateau <- !shift_large
  
  return(suitcase)
  
}



# Audit Suitcase (function)=========================================================================
audit_suitcase <- function(suitcase){
  # 1. Prepare stats (foundation for all audits)
  suitcase <- calc_dose_stats(suitcase)
  
  # 2.Run series of checks
  suitcase <- suitcase %>%
    audit_variance() %>%
    check_dose_range() %>%
    detect_outliers() %>%
    audit_monotonicity() #gatekeeper (sets Status)
  
  # 3. Conditionally final check
  #only check plateau if it hasn't already failed monotonicity
  if(suitcase$status=="Success"){
    suitcase <- check_plateau(suitcase)
  }
  
  return(suitcase)
  
}



# Create Plot=======================================================================================
visualize_audit <- function(suitcase){
  # Create theming
  status <- suitcase$status
  
  status_palettes <- list(
    "Success"=list(
      raw_points = "grey40", 
        means_lines = "deepskyblue4", 
        outliers = "red" 
      ), 
    "Failed" = list( 
      raw_points = "pink", 
      means_lines = "firebrick", 
      outliers = "orange") 
    )
  
  pal <- status_palettes[[status]]
  
  # Build caption
  cap_snr <- paste("SNR:",
                    round(suitcase$snr, 2),
                    "|")
  
  cap_status_msg <- paste0("Status: ",
                           suitcase$status,
                           "\n",
                           suitcase$message)
  
  cap <- if(!is.null(suitcase$snr)){
    paste(cap_snr, cap_status_msg)
  } else{cap_status_msg}

  
  # Extract and update suitcase DFs
  dose_min_shift <- suitcase$data %>%
    filter(dose!=0) %>%
    pull(dose) %>%
    min() %>%
    {./2}
  
  
  df_data <- suitcase$data %>%
    mutate(
      dose_up=ifelse(dose==0, dose_min_shift, dose),
      outlier=ifelse(id %in% suitcase$outlier_indices,
                     "outlier",
                     "non-outlier"
      )
    )
  
  df_stats <- suitcase$stats %>%
    mutate(
      dose_up=ifelse(dose==0, dose_min_shift, dose)
    )
  
  # Create plot
  df_data %>%
    ggplot() +
    geom_point(
      aes(x=dose_up,
          y=response,
          color=outlier,
          shape=outlier),
      alpha=0.4,
      show.legend=FALSE) +
    geom_pointrange(
      data=df_stats,
      aes(x=dose_up,
          y=mean_response,
          ymin=mean_response-sd_response,
          ymax=mean_response+sd_response),
      color=pal$means_lines) +
    geom_line(
      data=df_stats,
      aes(x=dose_up,
          y=mean_response),
      color=pal$means_lines
    ) +
    scale_color_manual(values=c("outlier"=pal$outliers,
                                "non-outlier"=pal$raw_points)) +
    scale_shape_manual(values=c("outlier"=4, "non-outlier"=19)) +
    scale_x_log10(
                  breaks=df_stats$dose_up,
                  labels=as.character(df_stats$dose),
                  minor_breaks=NULL) +
    labs(x=suitcase$dose_label,
         y=suitcase$response_label,
         caption=cap) +
    theme_bw() +
    theme(
      plot.caption=element_text(hjust=0)
    )
}



# # ARCHIVE===========================================================================================
# # Calculate Dose Stats------------------------------------------------------------------------------
# df_summary_cv <- df_ryegrass_filt %>%
#   group_by(dose) %>%
#   summarize(
#     n=n(),
#     mean_response=mean(response),
#     sd_response=sd(response),
#     cv_response=sd_response/mean_response
#   ) %>%
#   ungroup() %>%
#   mutate(cv_response_median=median(cv_response),
#          cv_comp=round(cv_response/cv_response_median, 1),
#          cv_comp_gt2=cv_comp>2)
# 
# 
# 
# # Audit Variance------------------------------------------------------------------------------------
# vec_cv_high <- df_summary_cv %>%
#   filter(cv_comp_gt2) %>%
#   pull(dose)
# 
# n_cv_high <- length(vec_cv_high)
# 
# str_cv_high <- dynamic_paste(n_cv_high, vec_cv_high)
# 
# 
# msg_high_cv <- paste("Warning: High replicate variance detected at",
#                      if(n_cv_high==1){"Dose"} else{"Doses"},
#                      paste0(str_cv_high, "."),
#                      "This dose is significantly noisier than the rest of the experiment."
# )
# 
# 
# 
# # Audit Distribution of Doses-----------------------------------------------------------------------
# max_dose <- max(df_ryegrass_filt[['dose']])
# 
# min_dose <- df_ryegrass_filt %>%
#   filter(dose!=0) %>%
#   pull(dose) %>%
#   min()
# 
# dose_range <- max_dose/min_dose
# 
# if(dose_range<10){
#   msg_dose_range <- "Limited dose range detected. Curve estimation may be unstable."
# }
# 
# 
# 
# # Detect Outliers-----------------------------------------------------------------------------------
# df_outliers <- df_ryegrass_filt %>%
#   add_count(dose) %>%
#   # filter(n>=4) %>%
#   group_by(dose) %>%
#   mutate(
#     Q1=quantile(response, 0.25),
#     Q3=quantile(response, 0.75),
#     IQR=Q3-Q1,
#     thresh_min=Q1-(1.5*IQR),
#     thresh_max=Q3+(1.5*IQR),
#     outlier=response < thresh_min|response > thresh_max
#   ) %>%
#   ungroup() %>%
#   #retain only outliers
#   filter(outlier) %>%
#   group_by(dose) %>%
#   mutate(n_outliers=n()) %>%
#   ungroup() %>%
#   distinct(id, dose, n_outliers)
# 
# 
# #2) Create warning message
# #deconstruct df
# vec_outlier_n_dose <- df_outliers %>%
#   distinct(dose, n_outliers) %>%
#   pull(n_outliers, name='dose') 
# 
# msg_outliers_body <- purrr::imap_chr(
#   vec_outlier_n_dose,
#   ~paste0(.x, " potential outliers detected at Dose ", .y, ".\n")
# )
# 
# msg_outliers <- paste("Note:\n",
#                       paste(msg_outliers_body, collapse=" "),
#                       "Model was fit using all data."
# )
# 
# 
# 
# # Audit Monotonicity--------------------------------------------------------------------------------
# df_monotonicity <- df_summary_cv %>%
#   summarize(
#     #response range
#     response_span=max(mean_response) - min(mean_response),
#     #range of responses for control vs max dose
#     net_change=mean_response[dose==min(dose)]-
#       mean_response[dose==max(dose)],
#     monotonicity_failture=abs(delta_anchors)<0.10*response_span
#   )
# 
# status_monotonicity <- df_monotonicity %>%
#   pull(monotonicity_failture)
# 
# diff_control_max <- df_monotonicity %>%
#   pull(delta_anchors_pct)
# 
# 
# if(status_monotonicity){
#   msg_monotonicity <- paste("Non-monotonic trend detected: Highest dose mean is",
#                             paste0(delta_anchors_pct, "%"),
#                             "higher than control."
#   )
# }
# 
# 
# 
# # Create Plot---------------------------------------------------------------------------------------
# plot_ryegrass <- ryegrass %>%
#   rename(dose=conc, response=rootl) %>%
#   # mutate(dose_mod=!!sym(col_dose)) %>%
#   mutate(dose_mod=ifelse(dose==0, 0.1, dose)) %>%
#   ggplot() +
#   geom_point(aes(x=dose_mod, 
#                  y=response,
#                  group=dose_mod)) +
#   geom_vline(xintercept=0.4, color="red", linetype="dashed") +
#   scale_x_log10() +
#   labs(x="conc",
#        y="rootl") +
#   theme_bw() +
#   geom_point(aes(x=dose_mod, y=response),
#              shape=4, color="red", size=2)
# #note: need theming function/object
# #consider adding a mean + error bars (as an I-beam)
# 
# #add a line to the scatter plot that connects the means
# #flag outliers on the plot (e.g., using a red 'X")









