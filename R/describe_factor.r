#' Computes a frequency table for factor variables
#' 
#' This function takes a factor variable (numeric variables are transformed into a factor variable) and computes a basic frequency table. 
#' 
#' @param x A numeric or factor variable.
#' @param name A name for the first colum (if not specified, it remains the variable code).
#' @param labels If you are evaluating a numeric vector, the function will transform it into a factor variable. You can specify custom labels for the factor levels here.
#' @param col_sum Do you want column sums?
#' @return A tibble. 
#' @examples 
#' x <- rep(c("female", "male"), 100)
#' 
#' describe_factor(x, name = "gender")
#' @export
describe_factor <- function(x,
                            name = NULL,
                            labels = NULL,
                            col_sum = TRUE){
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
  
  if (isTRUE(col_sum)) {
    temp <- temp %>% 
      bind_rows(temp %>% 
                  summarize(n = sum(n),
                            percent = sum(percent)) %>%
                  mutate(x = "sum") %>%
                  select(x, n, percent))
  }
  
  if (!is.null(name)) {
    temp <- temp %>%
      set_colnames(., c(name, "n", "percent"))
  }
    
  return(temp)
}
