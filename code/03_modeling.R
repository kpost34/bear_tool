# Model Fitting and Selection and Parameter/Metric Extraction Functions


# Initialize Modelling Audit Table==================================================================
init_modeling_audit <- function(suitcase){
  #create empty DF
  df <- tibble(
    model=c("BC.5", "LL.4", "LL.3", "LM", "Null"),
    converged=rep(NA, 5),
    aic=rep(NA, 5),
    slope_p=rep(NA, 5)
  )
  
  #update & return suitcase
  suitcase$modeling$audit <- df
  
  return(suitcase)
}



# Fit Model Safely==================================================================================
fit_safely <- function(suitcase, mod_name){
  df <- suitcase$data
  
  # Store error message & model as function
  mod_name_clean <- str_remove(mod_name, "\\.")
  msg_error <- paste("Error:", mod_name_clean, "model did not converge.")
  
  model_fn <- match.fun(mod_name)
  
  # Fit model
  #drc model
  if(mod_name %in% c("BC.5", "LL.4", "LL.3")){
    mod <- tryCatch(
      {
      drm(response ~ dose, data = df, fct = model_fn())
    }, error=function(e) {
      message(msg_error)
      NULL
    })
  } else if(mod_name=="LM"){
    mod <- tryCatch(
      {
      lm(response ~ dose, data=df)
    }, error=function(e){
      message(msg_error)
      NULL
    })
    } else{
    mod <- tryCatch(
      {
      lm(response ~ 1, data=df)
      }, error=function(e){
        message(msg_error)
        NULL
    })
    }
    
  # Return model
  return(mod)
}
  


# Update Model Information==========================================================================
update_model_info <- function(suitcase, mod, mod_name){
  # Extract audit DF
  df <- suitcase$modeling$audit
  
  # Determine convergence
  convergence <- !is.null(mod)
  
  # If no convergence
  if(!convergence){
    #update & return suitcase
    df$converged[df$model==mod_name] <- convergence
    suitcase$modeling$audit <- df
    
    return(suitcase)
    
    # If converagence
  }else{
    #AIC
    aic_val <- AIC(mod)
    
    #slope p-value
    p_val <- if(mod_name %in% c("BC.5", "LL.4", "LL.3")){
      summary(mod)$coefficients["b:(Intercept)", "p-value"]
    } else if(mod_name=="LM"){
      summary(mod)$coefficients["dose", "PR(>|t|)"]
    } else{NA}
  
    #update & return suitcase
    df$converged[df$model==mod_name] <- convergence
    df$aic[df$model==mod_name] <- aic_val
    df$slope_p[df$model==mod_name] <- p_val
    
    suitcase$modeling$audit <- df
    
    return(suitcase)
  }
}



# Select Best Model=================================================================================
select_best_model <- function(suitcase){
  # Seed objs for logic tree
  winner_found <- FALSE
  slope_p_signif <- FALSE
  
  # Create audit table
  suitcase <- init_modeling_audit(suitcase)
  
  # Run sigmoid models
  #bc5
  mod_bc5 <- fit_safely(suitcase, mod_name="BC.5")
  suitcase <- update_model_info(suitcase, mod=mod_bc5, mod_name="BC.5")
  
  #ll4
  mod_ll4 <- fit_safely(suitcase, mod_name="LL.4")
  suitcase <- update_model_info(suitcase, mod=mod_ll4, mod_name="LL.4")
  
  #ll3
  mod_ll3 <- fit_safely(suitcase, mod_name="LL.3")
  suitcase <- update_model_info(suitcase, mod=mod_ll3, mod_name="LL.3")
  
  mods_list <- list(BC.5=mod_bc5, LL.4=mod_ll4, LL.3=mod_ll3)
  
  #Extract updated audit table & add to it
  df <- as_tibble(suitcase$modeling$audit) %>%
    mutate(
      n_param=case_when(
        model=="LM"                ~ 2,
        model=="Null"              ~ 1,
        str_detect(model, "BC|LL") ~ as.numeric(str_extract(model, "\\d$")),
        TRUE                       ~ NA_real_
      )
    ) %>%
    arrange(n_param)
  
  # Filter for converged sigmoid models
  df_sig <- df %>%
    filter(model %in% c("BC.5", "LL.4", "LL.3"),
           converged)
  
  # Make comparison
  #count number of sigmoids that converged
  n_sig <- nrow(df_sig)
  
  if(n_sig>0){
    #seed winner
    sig_winner <- df_sig$model[df_sig$n_param==min(df_sig$n_param)]
  
    # If 3 sigmoid converged
    if(n_sig==3){
      sig_complex <- df_sig$model[df_sig$n_param==5]
      sig_middle <- df_sig$model[df_sig$n_param==4]
      
      #compare simple vs middle
      if(df_sig$aic[df_sig$model==sig_middle] < (df_sig$aic[df_sig$model==sig_winner] - 2)){
        sig_winner <- sig_middle
      }
      
      #compare current winner vs complex
      if(df_sig$aic[df_sig$model==sig_complex] < (df_sig$aic[df_sig$model==sig_winner] - 2)){
        sig_winner <- sig_complex
      }
    }
  
    # If 2 sigmoids converged
    if(n_sig==2){
      sig_complex <- df_sig$model[df_sig$n_param==max(df_sig$n_param)]
      
      if(df_sig$aic[df_sig$model==sig_complex] < (df_sig$aic[df_sig$model==sig_winner] - 2)){
        sig_winner <- sig_complex
      }
    }
    
    # Assess p-value
    slope_p_signif <- df_sig$slope_p[df_sig$model==sig_winner] < 0.05
    
    # If slope_p significant
    if(slope_p_signif){
      #update winner_found
      winner_found <- TRUE
      
      #create winner_label
      sig_winner_label <- if(sig_winner=="BC.5"){
        "Brain-Cousens (Hormetic)"
      } else if(sig_winner=="LL.4"){
        "Log-Logistic (Standard)"
      } else if(sig_winner=="LL.3"){
        "Log-Logistic (Fixed Baseline)"
      }
      
      #create rationale string
      rationale_sig_txt <- if(n_sig>1){
        paste(sig_winner_label, "selected: Cleared 2-unit AIC threshold over simpler sigmoids;",
              "slope significant (p < 0.05)")
      } else if(n_sig==1){
        paste(sig_winner_label, "selected: Only converged sigmoid model; slope significant",
              "(p < 0.05)")
      }
      
      #update & return suitcase
      suitcase$modeling$winner <- mods_list[[sig_winner]]
      suitcase$modeling$winner_name <- sig_winner
      suitcase$modeling$winner_label <- sig_winner_label
      suitcase$modeling$rationale <- rationale_sig_txt
      
      return(suitcase)
    }
  }
    
  # No sigmoids converged or best was not significant
  if(!winner_found){
    # Run LM
    mod_lm <- fit_safely(suitcase, mod_name="LM")
    suitcase <- update_model_info(suitcase, mod=mod_lm, mod_name="LM")
    
    df_lm <- suitcase$modeling$audit
    
    if(!is.null(mod_lm)){
      slope_p_signif <- df_lm$slope_p[df_lm$model=="LM"] < 0.05
    }
    
    # If slope_p significant
    if(slope_p_signif){
      #update winner_found
      winner_found <- TRUE
      
      #create rationale string
      rationale_lm_txt <- if(n_sig==0){
        "Linear Trend selected: Sigmoidal models failed to converge; slope significant (p < 0.05)."
      } else if(n_sig>0) {
        "Linear Trend selected: Sigmoidal models converged but lacked significant slopes (p > 0.05); linear slope significant (p < 0.05)."
      }
      
      #update & return suitcase
      suitcase$modeling$winner <- mod_lm
      suitcase$modeling$winner_name <- "LM"
      suitcase$modeling$winner_label <- "Linear Trend"
      suitcase$modeling$rationale <- rationale_lm_txt
      
      return(suitcase)
      
    }
  }
  
  # If winner still not found
  if(!winner_found){
    # Run Null
    mod_null <- fit_safely(suitcase, mod_name="Null")
    suitcase <- update_model_info(suitcase, mod=mod_null, mod_name="Null")
    
    #update winner_found
    winner_found <- TRUE
    
    #create rationale string
    rationale_null_txt <- paste(
      "No Detectable Response: Models failed to converge or lacked a significant dose-response", 
      "slope (p > 0.05); defaulting to intercept-only model."
    )
    
    #update & return suitcase
    suitcase$modeling$winner <- mod_null
    suitcase$modeling$winner_name <- "Null"
    suitcase$modeling$winner_label <- "No Detectable Response"
    suitcase$modeling$rationale <- rationale_null_txt
    
    return(suitcase)
    
  }
}
  
  

# Extract Winning Stats=============================================================================
## Subfunction
rename_params <- function(names) { 
  dplyr::case_when( 
    str_starts(names, "b") ~ "slope_b", 
    str_starts(names, "c") ~ "lower_c", 
    str_starts(names, "d") ~ "upper_d", 
    str_starts(names, "f") ~ "f_param",
    TRUE                    ~ names 
    ) 
}


## Function
extract_winning_stats <- function(suitcase){
  # Extract model and model name
  mod <- suitcase$modeling$winner
  mod_name <- suitcase$modeling$winner_name
  df_data <- suitcase$data
  
  # Calculate metrics for DF
  #pseudo r2
  if(str_detect(mod_name, "LL|BC")){
    response_pred <- predict(mod)
    response_obs <- df_data[["response"]]
    r2_sigmoid <- cor(response_pred, response_obs)^2
  }
  
  #LM ED components
  if(mod_name=="LM"){
    intercept_lm <- coef(mod)[[1]] 
    slope_lm <- coef(mod)[[2]] 
    intercept_lm_se <- summary(mod)$coefficients["(Intercept)", "Std. Error"]
    slope_lm_se <- summary(mod)$coefficients["dose", "Std. Error"]
  }
  
  # Build DF
  ## Sigmoids
  if(str_detect(mod_name, "LL|BC")){
    #ed
    df_ed <- ED(mod, 50, interval="delta", display=FALSE) %>% 
      as_tibble() %>%
      clean_names() %>%
      #prevent negative lower bounds
      mutate(lower=ifelse(lower<0, 0, lower)) %>%
      set_names(paste("ed50", c("est", "se", "ci_low", "ci_high"), sep="_"))
    
    #parameters
    df_params <- coef(mod) %>%
      as_tibble_row() %>%
      rename_with(rename_params) %>%
      dplyr::select(!starts_with("e")) %>%
      #conditionally add d and f_param to LL models
      {if(mod_name=="LL.3") mutate(., upper_d=1) else .} %>%
      {if(str_detect(mod_name, "^LL")) mutate(., f_param=NA_real_) else .} 
  
    #metrics
    df_metrics <- tibble(
      aic=AIC(mod),
      r2_pseudo=r2_sigmoid,
      p_val_slope=summary(mod)$coefficients["b:(Intercept)", "p-value"],
      resid_std_error=summary(mod)$rseMat[1, 1]
    )
    # Non-sigmoids
  } else if(mod_name %in% c("LM", "Null")){
    if(mod_name=="LM"){
      ## LM 
      #ed
      df_ed <- tibble(ed50_est=ed50 <- (-0.5*intercept_lm)/slope_lm) %>%
        mutate(
          ed50_se=sqrt(
            ed50_est^2 * ((intercept_lm_se/intercept_lm)^2 + (slope_lm_se/slope_lm)^2)
          ),
          ed50_ci_low=ed50_est - (1.96*ed50_se),
          ed50_ci_high=ed50_est + (1.96*ed50_se)
        )
      
      # Null
    } else if(mod_name=="Null"){
      #ed
      df_ed <- tibble(
        ed50_est=NA_real_,
        ed50_se=NA_real_,
        ed50_ci_low=NA_real_,
        ed50_ci_high=NA_real_
      )
    }
    
      #parameters
      df_params <- tibble(
        slope_b=if(mod_name=="LM"){
          coef(mod)[["dose"]]} else{0},
        lower_c=NA_real_,
        upper_d=coef(mod)[[1]],
        f_param=NA_real_
      )
      
      #metrics
      df_metrics <- tibble(
        aic=AIC(mod),
        r2_pseudo=if(mod_name=="LM"){summary(mod)$r.squared} else{0},
        p_val_slope=if(mod_name=="LM"){
          summary(mod)$coefficients["dose", "Pr(>|t|)"]} else{NA_real_},
        resid_std_error=summary(mod)$sigma
      )
  }
    
  # All models: model info
  df_model <- tibble(
    model_type=mod_name,
    status=if(mod_name=="Null"){"No Response Detected"} else{"Response Detected"},
    rationale=suitcase$modeling$rationale
  )
  
  # Combine DFs
  df_results <- bind_cols(df_ed, df_params, df_metrics, df_model)
  
  # Update & return suitcase
  suitcase$modeling$results <- df_results
  
  return(suitcase)
  
}

    

# ARCHIVE===========================================================================================
# ## Parameters (b, c, d, e, f)
# coef(mod_final)
# 
# 
# ## ED50 values
# #ll3, ll4, or bc5
# ed50 <- ED(mod_final, 50, interval="delta") %>% 
#   as_tibble() %>%
#   clean_names() %>%
#   #prevent negative lower bounds
#   mutate(lower=ifelse(lower<0, 0, lower))
# 
# #lm
# mod_lm_summ <- summary(mod_lm)
# intercept <- coef(mod_lm)[1] %>% unname()
# slope <- coef(mod_lm)[2] %>% unname()
# ed50 <- (-0.5*intercept)/slope
# se_intercept <- mod_lm_summ$coefficients[1, "Std. Error"]
# se_slope <- mod_lm_summ$coefficients[2, "Std. Error"]
#   
# ed50_se <- sqrt(
#   ed50^2 * ((se_intercept/intercept)^2 + (se_slope/slope)^2)
# )
# 
# ed50_upper <- ed50 + (1.96*ed50_se)
# ed50_lower <- ed50 - (1.96*ed50_se)
# 
# 
# #R^2 calculation
# #psuedo-R^2 for drc mocels = correlation between obs and predicted values squared
# response_pred <- predict(mod_final)
# response_obs <- df_ryegrass_filt[["response"]]
# r2 <- cor(response_pred, response_obs)^2
# 
# r2 <- summary(mod_lm)$r.squared
# 
# 
# #final info (for a null model)
# #model_type: Null (intercept-only)
# #ed50_est: NA
# #ed50_se: NA
# #ed50_ci: [NA, NA]
# #slope_b: 0
# #upper_d: intercept
# #aic: AIC value of null model
# #f (only if final model is hormesis): NA
# #r2: 0
# #status: "No significant effect detected" (Covnerged successfully" for others)
# 
# 
# 
# 
# # ARCHIVE===========================================================================================
# # Model fitting & selection-------------------------------------------------------------------------
# df_ryegrass_filt <- ryegrass %>%
#   rename(dose="conc", response="rootl")
# 
# 
# ## Fit LL3, LL4 and Hormesis models
# # mod_formula <- as.formula(paste(col_response, "~", col_dose))
# # mod_formula_null <- as.formula(paste(col_response, "~", 1))
# 
# tryCatch({
#   mod_ll4 <- drm(response ~ dose, data = df_ryegrass_filt, fct = LL.4())
# }, error=function(e) {
#   msg_error <- "Error: LL4 model did not converge"
#   message(msg_error)
# })
# 
# tryCatch({
#   mod_bc5 <- drm(response ~ dose, data = df_ryegrass_filt, fct = BC.5())
# }, error=function(e) {
#   msg_error <- "Error: BC5 model did not converge"
#   message(msg_error)
# })
# 
# tryCatch({
#   mod_ll3 <- drm(response ~ dose, data = df_ryegrass_filt, fct = LL.3())
# }, error=function(e) {
#   msg_error <- "Error: LL3 model did not converge"
#   message(msg_error)
# })
# 
# 
# ## Compare AIC of models (more complex model retained if AIC is two units better than simpler one)
# AIC(mod_ll4)
# AIC(mod_bc5)
# AIC(mod_ll3)
# 
# # status_comp1 <- if(AIC(mod_bc5) < AIC(mod_ll4)-2){
# #   "BC5 is the better fit model"
# # } else{
# #   "LL4 is better fit model"
# # }
# 
# ## Look at p-value of parameter b
# 
# 
# 
# ## LM
# #fit this model if best model fails slope check
# tryCatch({
#   mod_lm <- lm(response ~ dose, data=df_ryegrass_filt)
# }, error=function(e) {
#   msg_error <- "Error: Linear model did not converge"
#   message(msg_error)
# })
# 
# 
# #if it fits, then look at p-value; if slope p-value is significant, then this is best model;
# #otherwise it is null model
# #calculate mean of response
# mod_null <- lm(response ~ 1, data=df_ryegrass_filt)
# final_model <- "Null"
# msg_final_model <- "No relationship was found between ___ and ____"
# 
# mod_final <- mod_ll4









