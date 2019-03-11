#' A frequency table for factor variables
#' 
#' This function takes a factor variable (numeric variables are transformed into a factor variable) and computes a basic frequency table. Column sums are added automatically, but can be removed if desired. By default, NAs will be removed before computation. Yet, arguments that are usally passed to the \code{\link[base]{table}} function can be added to also count NAs (useNA = "always"). 
#' 
#' @param x A numeric or factor variable.
#' @param name A name for the first colum (if not specified, it remains the variable code).
#' @param labels If you are evaluating a numeric vector, the function will transform it into a factor variable. You can specify custom labels for the factor levels here.
#' @param col_sum Logical value indicating whether column sums should be returned
#' @param ... Further arguments that can be passed to \code{\link[base]{table}}. This has been primarily included to allow to count NAs. In this case, simply add the following argument: \code{useNA = "always"}
#' @return The function \code{describe_factor()} returns a contigency table with relative and absolute values and an object of class \code{tbl_df} (a data frame).
#' @examples 
#' # Standard example
#' d <- mtcars
#' describe_factor(d$cyl, name = "No. of cylinders")
#' 
#' # Example with missing values
#' d$cyl[3] <- NA # Creating a missing value
#' describe_factor(d$cyl, 
#'                 name = "No. of cylinders", 
#'                 useNA = "always") %>% class
#' @export
describe_factor <- function(x,
                            name = NULL,
                            labels = NULL,
                            col_sum = TRUE,
                            ...){
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
      table(., ...) %>%
      as.tibble %>%
      rename(x = ".")
    
    temp <- x %>%
      table(., ...) %>%
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
