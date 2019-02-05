#' Computes a frequency table for factor variables
#' 
#' This function takes a factor variable (numeric variables are transformed into a factor variable) and computes basic frequency table.
#' 
#' @param x A numeric or factor variable.
#' @param name A name for the first colum (if not specified, it remains the variable code).
#' @param labels If you are evaluating a single vector, you specify the variable name here.
#' @examples 
#' x <- rep(c("female", "male"), 100)
#' 
#' describe_factor(x, name = "gender")
#' @export
describe_factor <- function(x,
                            name = NULL,
                            labels = NULL){
  # dependencies
  library(tidyverse)
  library(magrittr)
  
  # function
  if (!is.null(labels)) {
    x <- factor(x, 
                labels = labels)
  } else {
    x <- factor(x)
  }
    
    n <- x %>%
      table %>%
      as.tibble %>%
      rename(x = ".")
    
    temp <- x %>%
      table %>%
      prop.table %>% 
      as.tibble %>%
      rename(x = ".") %>%
      mutate(percent = n*100) %>%
      select(x, percent)
    
    temp <- left_join(temp, n) %>%
      select(x, n, percent)
  
  if (!is.null(name)) {
    temp <- temp %>%
      set_colnames(., c(name, "n", "percent"))
  }
    
  return(temp)
}
