# Curve Generation, Final Visualization, and Summary Table Functions


# Generate Curve====================================================================================
#' Generate High-Resolution Model Predictions
#'
#' Creates a smooth prediction curve for visualization by generating 100 
#' log-spaced dose points across the experimental range. It handles the "Zero 
#' Dose" problem by shifting the lower bound to 50% of the lowest non-zero 
#' dose, ensuring the curve extends naturally on log-scale axes.
#'
#' @param suitcase List. Must contain the validated 'data' and the 'winner' 
#' model object.
#'
#' @return An updated suitcase containing \code{modeling$predictions}: 
#' A tibble of 100 points with columns:
#' \itemize{
#'   \item \code{dose}: Log-spaced dose levels.
#'   \item \code{prediction}: The fitted response value.
#'   \item \code{lower} / \code{upper}: 95% confidence intervals for the fit.
#' }
generate_prediction_data <- function(suitcase){
  
  # Extract parts of suitcase
  df_data <- suitcase$data
  mod <- suitcase$modeling$winner
  
  # Shift control dose & get max dose
  dose_min_shift <- df_data %>%
    filter(dose!=0) %>%
    pull(dose) %>%
    min() %>%
    {./2}
  
  dose_max <- max(df_data$dose)
  
  # Log10-transform min/max, create sequence, and back-transform
  dose_min_log <- log10(dose_min_shift)
  dose_max_log <- log10(dose_max)
  
  dose_curve_log <- seq(dose_min_log, dose_max_log, length.out=100)
  
  dose_curve <- 10^(dose_curve_log)
  
  # Build DF of dose-response data
  df_dose_curve <- data.frame(dose=dose_curve)
  
  df_response_curve <- suppressWarnings(
    predict(mod, newdata=df_dose_curve, interval="confidence") %>%
      as.data.frame()
  )
  
  #normalize column names for lm() and Null models
  if("fit" %in% names(df_response_curve)) {
    df_response_curve <- df_response_curve %>%
      dplyr::rename(prediction="fit", lower="lwr", upper="upr")
  }
  
  df_curve <- bind_cols(
    df_dose_curve,
    df_response_curve
  ) %>%
    clean_names() %>%
    as_tibble()
  
  # Update & return suitcase
  suitcase$modeling$predictions <- df_curve
  
  return(suitcase)
}



# Visualize Results=================================================================================
#' Generate Primary Dose-Response Visualization
#'
#' Constructs a publication-quality ggplot2 object. The plot layers raw 
#' observations, the fitted model line, and a 95% confidence interval ribbon. 
#' It utilizes a log-transformed x-axis and dynamically places statistical 
#' annotations (ED50 and R-squared) based on the model's performance.
#'
#' @param suitcase List. Must contain 'data', 'predictions', 'results', and 'metadata'.
#'
#' @return An updated suitcase containing \code{modeling$plot}: A ggplot object 
#' with the following features:
#' \itemize{
#'   \item \code{geom_ribbon}: Represents the 95% confidence interval of the fit.
#'   \item \code{geom_line}: The smooth prediction curve across the dose range.
#'   \item \code{scale_x_log10}: Standardized log-scale with breaks set to actual doses.
#'   \item \code{annotate}: Mathematical expressions for ED50 and R-squared.
#' }
visualize_results <- function(suitcase){
  
  # Extract & store obj related to data
  mod_name <- suitcase$modeling$winner_name
  df_curve <- suitcase$modeling$predictions
  
  dose_min_shift <- suitcase$data %>%
    filter(dose!=0) %>%
    pull(dose) %>%
    min() %>%
    {./2}
  
  df_data <- suitcase$data %>%
    mutate(dose=ifelse(dose==0, dose_min_shift, dose))
  
  dose_max <- max(df_data$dose)
  
  doses <- distinct(df_data, dose) %>%
    pull(dose)
  
  # Create labels
  dose_lab <- suitcase$metadata$x_label
  response_lab <- suitcase$metadata$y_label
  
  # Build annotations
  ed50 <- suitcase$modeling$results$ed50_est
  ed50_lab <- if(!is.na(ed50)){
    ## Case 1: Success
    paste0("ED[50]*':'~", round(ed50, 2))
  } else if(mod_name=="Null") {
    ## Case 2: True Null model
    "No~Significant~Response"
  } else {
    ## Case 3: Valid model (like BC), but ED calculation failed
    "ED[50]*':'~Inestimable"
  }
  
  r2 <- suitcase$modeling$results$r2_pseudo
  r2_lab <- paste0("R^2*':'~", round(r2, 3))
  
  # Create plot
  plot_obj <- df_data %>%
    ggplot() +
    geom_ribbon(data=df_curve, aes(x=dose, ymin=lower, ymax=upper),
                color=NA, fill="firebrick", alpha=0.15) +
    geom_line(data=df_curve, aes(x=dose, y=prediction),
              linewidth=1.2) +
    geom_point(aes(x=dose, y=response),
               color="gray40", alpha=0.6) +
    scale_x_log10(
      breaks=doses,
      labels=c("0", as.character(doses[-1])),
      minor_breaks=NULL
      ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    annotate(geom="label", x=max(df_curve$dose), y=max(df_curve$upper), hjust=1, vjust=1,
             label=ed50_lab, parse=TRUE) +
    {if(mod_name!="Null")
      annotate(geom="label", x=max(df_curve$dose), y=max(df_curve$upper)*0.9, hjust=1, vjust=1,
               label=r2_lab, parse=TRUE)
      } +
    labs(x=dose_lab,
         y=response_lab) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  # Update & return suitcase
  suitcase$modeling$plot <- plot_obj

  return(suitcase)
}



# Get Summary Table=================================================================================
# Formats model results into a publication-ready interactive table (DT) featuring Unicode math 
  #symbols and selection rationale
get_summary_table <- function(suitcase){
  
  # Extract results
  df_results <- suitcase$modeling$results
  
  # Build summary table
  df_table <- df_results %>%
    mutate(
      model_name=case_when(
        model_type=="BC.5" ~ "Brain-Cousens (Hormetic)",
        model_type=="LL.4" ~ "Log-Logistic (Standard)",
        model_type=="LL.3" ~ "Log-Logistic (Fixed Baseline)",
        model_type=="LM"   ~ "Linear Trend",
        model_type=="Null" ~ "No Detectable Response",
        TRUE               ~ "NEEDS CATEGORY"
      ),
      ed50_ci=paste0(
        "(", round(ed50_ci_low, 2), " - ", round(ed50_ci_high, 2), ")"
      ),
      ed50_interval=ifelse(model_type=="Null", "N/A", paste(round(ed50_est, 2), ed50_ci)),
      r2_pseudo=round(r2_pseudo, 3)
    ) %>%
    dplyr::select(model_name, ed50_interval, r2_pseudo, rationale) %>%
    set_names(
      nm=c("Analysis Model", 
           paste("ED\u2085\u2080 Interval"), 
           paste("Goodness of Fit (R\u00b2)"),
           "Decision Rationale"
      )
    ) %>%
    DT::datatable(
      rownames=FALSE,
      caption="Table 1: Potency Summary and Model Fit",
      options=list(
        dom="t",
        ordering=FALSE,
        paging=FALSE,
        searching=FALSE
      )
    )
  
  # Update & return suitcase
  suitcase$modeling$table <- df_table
  
  return(suitcase)
}


