#' Count overall amount of missing values
#' 
#' This function counts the overall amount of missing values in a data set and returns a printable table. 
#' 
#' @param data A data frame.
#' @param percent A logical value indicating whether to compute also the percentage (defaults to TRUE).
#' 
#' @return A tibble..
#' @examples
#' d <- mtcars
#' d[4,3] <- NA # Create missing values to illustrate function
#' 
#' count_na(d, percent = FALSE)
#' count_na(d)
#' @export
count_na <- function(data, percent = TRUE) {
  
  # dependencies
  library(dplyr)
  library(magrittr)
  
  # number of missings
  n <- data %>%
    is.na %>%
    table %>%
    as.tibble
  
  # percent of missings
  perc <- data %>%
    is.na %>%
    table %>%
    prop.table %>%
    as.tibble %>%
    mutate(percent = n*100) %>%
    select(".", percent)
  
  if (percent != TRUE) {
    temp <- n %>%
      set_colnames(c("missings", "n"))
    
    return(temp)
    
  } else {
    temp <- left_join(n, perc) %>%
      set_colnames(c("missings", "n", "percent"))
    
    return(temp)
    
  }
}




