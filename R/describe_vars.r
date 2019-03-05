#' Customized version of the function describe (psych) for single and multiple numeric variables
#' 
#' This function is essentially a customized version of the function \code{describe()} included the package \code{psych}, but provides some additional benefits: First, it transforms the the output into a tibble. Second, it reduces the number of psychometrics down to the most important ones. Third, it allows to include item formulations drawn from an additional data frame (needs to have a variable "code" that includes similar variable codes as the original data frame and a variable named "item" that includes the item formulations). Overall, it provides enough functionality to create a printable "Table 1". 
#' 
#' @param data A data frame that includes that variables that should be evaluated. Can also be a vector with numeric values. 
#' @param items A data frame containing the variable codes and item formulations.
#' @param var_name If you are evaluating a single vector, you specify the variable name here. 
#' @param brief A logical value indicating wether all psychometrics or only the mean and the standard deviations should be printed
#' @param first_col How should the first column be named? Defaults to "code.
#' @param ... Further arguments that can be passed to \code{describe}.
#' @return A tibble. 
#' @examples 
#' d <- mtcars
#' 
#' describe_vars(mtcars)
#' describe_vars(mtcars$cyl, var_name = "cylinder")
#' @export
describe_vars <- function(data, 
                          items = NULL, 
                          var_name = NULL, 
                          brief = FALSE, 
                          first_col = "code",
                          ...) {
  
  # dependencies
  library(psych)
  library(tidyverse)
  
  temp <- data %>%
    describe(., ...) %>%
    as.data.frame %>%
    rownames_to_column(first_col) 
  
  if (!is.null(items)) {
    temp <- temp %>%
      left_join(items) %>%
      select(first_col, item, mean, sd, min, max, skew, kurtosis, n)
    
  } else {
    
    temp <- temp %>%
      select(first_col, mean, sd, min, max, skew, kurtosis, n)
  }
  
  if((is.element("integer", class(data)) | is.element("numeric", class(data))) & !is.null(var_name)){
    temp <- temp %>%
      mutate(variable = var_name) %>%
      select(variable, everything(), -first_col)
  }
  
  if (isTRUE(brief) & !is.null(items)) {
    temp %>%
      select(first_col, item, mean, sd) %>%
      as.tibble
  } else if (isTRUE(brief) & is.null(items)){
    temp %>%
      select(first_col, mean, sd) %>%
      as.tibble
  } else {
    temp %>% 
      as.tibble
  }
}


