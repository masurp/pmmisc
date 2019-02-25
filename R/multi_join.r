#' Customized version of the function inner_join() (dplyr) for more than two data frames
#' 
#' This function joins up to five data sets using the function \code{inner_join} from the package "dplyr". Each data set is individually joined with the previous one. This way, several data sets of which only pairs have the same id variable can be joined. 
#' 
#' @param ... Up to five data sets that should be joined. 
#' @return A data frame.
#' @examples 
#' # Creating several data frames.
#' id <- c(1:100)
#' var1 <- runif(100, 1, 5)
#' var2 <- runif(100, 1, 5)
#' var3 <- runif(100, 1, 5)
#' data1 <- data.frame(id, var1)
#' data2 <- data.frame(id, var2)
#' data3 <- data.frame(id, var3)
#'
#' # Join all of them
#' multi_join(data1, data2, data3)
#' @export
multi_join <- function(...){
  
  # dependencies
  if(!require(dplyr)){install.packages('dplyr')}
  library(dplyr)
  
  # function
  df_list <- lapply(list(...), as.tibble)
  
  if (length(df_list) == 1) {
    message("You need to provide at least to data frames!")
  } else if (length(df_list) == 2) {
    data <- inner_join(df_list[[1]], df_list[[2]])
  } else if (length(df_list) == 3) {
    data <- df_list[[1]] %>%
      inner_join(df_list[[2]]) %>% 
      inner_join(df_list[[3]])
  } else if (length(df_list) == 4) {
    data <- df_list[[1]] %>% 
      inner_join(df_list[[2]]) %>% 
      inner_join(df_list[[3]]) %>%
      inner_join(df_list[[4]])
  } else if (length(df_list) == 5) {
    data <- df_list[[1]] %>% 
      inner_join(df_list[[2]]) %>% 
      inner_join(df_list[[3]]) %>%
      inner_join(df_list[[4]]) %>%
      inner_join(df_list[[5]])
  }
  return(data)
}


