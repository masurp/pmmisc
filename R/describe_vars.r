#' Customized version of the function describe (psych) for single and multiple numeric variables
#' 
#' This function is essentially a customized version of the function \code{describe()} included the package \code{psych}, but provides some additional benefits: First, it transforms the the output into a tibble. Second, it reduces the number of psychometrics down to the most important ones. Third, it allows to include item formulations drawn from an additional data frame (needs to have a variable "code" that includes similar variable codes as the original data frame and a variable named "item" that includes the item formulations). Overall, it provides enough functionality to create a printable "Table 1". 
#' 
#' @param data A data frame that includes that variables that should be evaluated. Can also be a vector with numeric values. 
#' @param items A data frame containing the variable codes (first column "code") and item formulations (second column "item").
#' @param var_name If you are evaluating a single vector, you specify the variable name here. 
#' @param brief A logical value indicating wether all psychometrics or only the mean and the standard deviations should be printed
#' @param first_col How should the first column be named? Defaults to "code".
#' @param n Logical value indicating whether the number of valid cases should be included in the output.
#' @param print Logical value indicating whether the output should be formated according to APA-guidelines (if true, the output can easily be printed with the function \code{\link[papakja]{apa_table}}).
#' @param digits A numeric value indicating how many digits should be printed (only works together with \code{print = TRUE})
#' @param ... Further arguments that can be passed to \code{describe}.
#' @return A data frame with relevant item statistics such as mean, standard deviation, minimum, maximum, skewness and kurtosis.
#' @examples 
#' describe_vars(mtcars)
#' describe_vars(mtcars, n = TRUE, print = TRUE, digits = 3)
#' describe_vars(mtcars$cyl, var_name = "cylinder")
#' @export
describe_vars <- function(data, 
                          items = NULL, 
                          var_name = NULL, 
                          brief = FALSE, 
                          first_col = "code",
                          n = FALSE,
                          print = FALSE,
                          digits = 2,
                          ...) {
  
  # dependencies
  library(psych)
  library(tidyverse)
  library(papaja)
  
  temp <- data %>%
    describe(., ...) %>%
    as.data.frame %>%
    rownames_to_column(first_col) %>%
    as_tibble
  
  if (!is.null(items)) {
    items$code <- as.character(items$code)
    temp <- temp %>%
      left_join(items) %>%
      select(first_col, item, mean, sd, min, max, skew, kurtosis, n)
    
  } else {
    
    temp <- temp %>%
      select(first_col, mean, sd, min, max, skew, kurtosis, n)
  }
  
  if((is.element("integer", class(data)) | 
      is.element("numeric", class(data))) & 
     !is.null(var_name)){
    
    temp <- temp %>%
      mutate(variable = var_name) %>%
      select(variable, everything(), -first_col)
  }
  
  if (isTRUE(brief) & !is.null(items)) {
    temp <- temp %>%
      select(first_col, item, mean, sd) %>%
      as.tibble
  }
  
  if (isTRUE(brief) & is.null(items)){
    temp <- temp %>%
      select(first_col, mean, sd) %>%
      as.tibble
  } 
  
  if (!isTRUE(n)) {
    temp <- temp %>%
      select(-n)
  }
  
  if (isTRUE(print)) {
    temp <- temp %>%
      mutate_at(vars(mean:kurtosis), 
                funs(printnum(., digits = digits)))
    
  }
  
  return(temp)
}


