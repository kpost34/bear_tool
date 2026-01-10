# Function to Inject User-Friendly Names and Labels into Suitcase


add_metadata <- function(suitcase, x_name, x_unit, y_name, y_unit) {
  
  # Construct strings and store components
  suitcase$metadata <- list(
    x_name=x_name,
    y_name=y_name,
    x_unit=x_unit,
    y_unit=y_unit,
    x_label=ifelse(is.na(x_unit), x_name, paste0(x_name, " (", x_unit, ")")),
    y_label=ifelse(is.na(y_unit), y_name, paste0(y_name, " (", y_unit, ")"))
  )
  
  return(suitcase)
}

