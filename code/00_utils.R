# Helper Functions Used Throughout Project


## Function to dynamically combine elements into a string
dynamic_paste <- function(vec, add_quotes=TRUE){
  n <- length(vec)
  if(add_quotes){
    vec <- purrr::map_chr(vec, function(x) paste0("'", x, "'"))
  }
  vec_str <- if(n==1) {
    vec
  } else if(n==2){
    paste(vec, collapse=" and ")
  } else if(n>2){
    paste(vec, collapse=", ") %>%
      str_replace(", (?=[^,]*$)", " and ")
  }
  
  return(vec_str)
}


## Function to stack messages without producing empty lines
append_message <- function(msg, msg_new){
  if(msg==""){
    msg_new
  } else{
    paste(msg, msg_new, sep="\n")
  }
}



