# Dataset Auditing Functions


# Calculate Dose Stats==============================================================================
# Aggregates group-wise metrics (Mean, SD, CV, IQR) for diagnostic auditing and outlier detection
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
# Identifies doses with excessive replicate noise by comparing individual CVs to the global median
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
# Calculates the ratio between max/min doses to warn of limited ranges that may destabilize curve 
  #fitting
check_dose_range <- function(suitcase){
  df <- suitcase$data
  
  # Store objs
  dose_max <- max(df$dose)
  
  dose_min <- df %>%
    filter(dose!=0) %>%
    pull(dose) %>%
    min()
  
  dose_range <- dose_max/dose_min
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
  
  # Return suitcase
  return(suitcase)
}



# Detect Outliers===================================================================================
# Flags individual observations as outliers using the 1.5 * IQR (Tukey's) rule per dose group
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
# Assesses biological signal strength (SNR) and net direction; fails the suitcase if the response 
  #is non-monotonic or flat
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
  
  # Calculate pooled SNR & assign it and direction to suitcase
  snr <- df_mono$span/mean(df$sd_response)
  suitcase$direction <- df_mono$direction
  suitcase$snr <- snr
  
  # Flat line
  if(df_mono$delta==0){
    
    #store objs
    mono_status <- "Failed"
    mono_msg <- "Error: No response detected. The mean response at the highest dose is identical
                to the control"
    
    #print warning
    warning(mono_msg)
    
    #update suitcase
    suitcase$status <- mono_status
    suitcase$message <- append_message(suitcase$message, mono_msg)
    
    # Non-monotonicity
  } else if(df_mono$mono_fail){
    
    #store objs
    mono_status <- "Failed"
    mono_msg <- "Error: Non-monotonic data detected. Net displacement is insufficient relative to 
                 total variance"
    
    #print warning
    warning(mono_msg)
    
    #update suitcase
    suitcase$status <- mono_status
    suitcase$message <- append_message(suitcase$message, mono_msg)
    
    # Low SNR
  } else if(snr<3){
    
    #store message
    snr_msg <- "Warning: Low Signal-to-Noise Ratio (SNR < 3). Curve fit may be unreliable."
    
    #print warning
    warning(snr_msg)
    
    #update suitcase
    suitcase$message <- append_message(suitcase$message, snr_msg)
    
  } 
  
  # Return suitcase
  return(suitcase)

}



# Audit Plateau=====================================================================================
# Evaluates the terminal slope of the data to determine if a stable top/bottom plateau was reached
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
    plateau_msg <- "Warning: Terminal plateau not reached. EC50 estimation may be extrapolated."
    
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
#' Perform Statistical Diagnostics and Data Quality Audit
#'
#' Evaluates the statistical properties of the dataset to determine if it is 
#' suitable for non-linear dose-response modeling. This function calculates 
#' group-wise statistics and runs a battery of quality checks.
#'
#' @param suitcase List. The object initialized by `validate_data()` containing 
#' the standardized tibble and project metadata.
#'
#' @return An updated 'suitcase' (list) containing new diagnostic elements:
#' \itemize{
#'   \item \code{stats}: A summary tibble of means, SDs, CVs, and IQRs per dose.
#'   \item \code{high_cv_doses}: A vector of doses where replicate variance is double the median.
#'   \item \code{outlier_indices}: A vector of IDs flagged using the 1.5 * IQR rule.
#'   \item \code{snr}: The Signal-to-Noise Ratio (Span of means / Pooled SD).
#'   \item \code{direction}: "Inhibition" or "Activation" based on net response.
#'   \item \code{at_plateau}: Logical. Indicates if the response reached a terminal plateau.
#'   \item \code{status}: Updated to "Failed" if data is non-monotonic or lacks a signal.
#' }
audit_suitcase <- function(suitcase){
  
  # 1. Prepare stats (foundation for all audits)
  suitcase <- calc_dose_stats(suitcase)
  
  # 2. Run series of quality checks
  # Note: audit_monotonicity acts as the Gatekeeper for suitcase$status
  suitcase <- suitcase %>%
    audit_variance() %>%
    check_dose_range() %>%
    detect_outliers() %>%
    audit_monotonicity() 
  
  # 3. Final Plateau Check 
  # Only assessed if the dataset shows a monotonic trend
  if(suitcase$status == "Success"){
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
  
  # Create labels
  dose_lab <- suitcase$metadata$x_label
  response_lab <- suitcase$metadata$y_label
  
  # Create plot
  plot_obj <- df_data %>%
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
      minor_breaks=NULL
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(x=dose_lab,
         y=response_lab
    ) +
    theme_bw() +
    theme(
      axis.text.x=element_text(angle=45, hjust=1),
      plot.caption=element_text(hjust=0)
    )
  
  # Update & return suitcase
  suitcase$audit_plot <- plot_obj
  
  return(suitcase)
}



