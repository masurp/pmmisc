#' Creates table with sample sizes and attrition rates
#' 
#' This functions binds individual numeric values (representing sample sizes) in a printable table. It is a follow-up function for \code{save_n} which extracts the number of cases in a data frame.  
#' 
#' @param ... Several numeric values representing sample sizes.
#' @param sample Names for the rows in the final table
#' @return An object of class \code{tibble} that can be passed to \code{apa_table()}. It contains all sample sizes that were provided as individual numeric values. 
#' @examples
#' # Simple example
#' sample_sizes(100, 80, 10)
#' 
#' # Realistic example
#' d <- mtcars
#' d1 <- d[1:28,]
#' d2 <- d1[1:20,]
#' 
#' n0 <- save_n(d)
#' n1 <- save_n(d1)
#' n2 <- save_n(d2)
#' 
#' sample_sizes(n0, n1, n2)
#' @export
sample_sizes <- function(..., 
                         sample = c("Overall clicks",
                                    "Eligible cases",
                                    "Complete cases")) {
  # dependencies
  library(dplyr)
  
  # creating table
  temp <-cbind(
    sample,
    n = unlist(list(...))
    ) %>%
    as.tibble %>%
    mutate(n = as.numeric(n),
           percent = (n/unlist(...)[[1]])*100)
  return(temp)
}



