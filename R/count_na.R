#' Amount of missing values in a data set
#' 
#' This function produces a table with the overall amount of missing values in a data set (absolute and relative values). 
#'
#' @param data A data frame.
#' @param percent A logical value indicating whether to compute also the percentage (defaults to TRUE).
#' @param col_sum A logical value indicating whether the column sums should be returned. 
#' @return A tibble.
#' @examples
#' d <- mtcars
#' d[4,3] <- NA # Create missing values to illustrate function
#' 
#' count_na(d, percent = FALSE)
#' count_na(d, col_sum = FALSE)
#' @export
count_na <- function(data, 
                     percent = TRUE,
                     col_sum = TRUE) {
  # dependencies
  library(tidyverse)
  library(magrittr)
  
  # number of missings
  n <- data %>%
    is.na %>%
    table %>%
    as_tibble
  
  # percent of missings
  perc <- data %>%
    is.na %>%
    table %>%
    prop.table %>%
    as_tibble %>%
    dplyr::mutate(percent = n*100) %>%
    dplyr::select(".", percent)
  
  if (percent != TRUE) {
    temp <- n %>%
      set_colnames(c("missings", "n"))
    
    if (isTRUE(col_sum)) {
      temp <- temp %>% 
        bind_rows(temp %>% 
                    dplyr::summarize(n = sum(n)) %>%
                    dplyr::mutate(missings = "sum") %>%
                    dplyr::select(missings, n))
    }
    
  } else {
    temp <- left_join(n, perc) %>%
      set_colnames(c("missings", "n", "percent"))
    
    if (isTRUE(col_sum)) {
      temp <- temp %>% 
        bind_rows(temp %>% 
                    dplyr::summarize(n = sum(n),
                              percent = sum(percent)) %>%
                    dplyr::mutate(missings = "sum") %>%
                    dplyr::select(missings, n, percent))
    }
    
  }
  
  return(temp)
}




