#' Socio-demographics table
#' 
#' This function takes socio-demographic variables and transforms them into a printable table. A numeric variable representing age (in years) will be summarized as mean and standard deviation. Gender and education will be returned as frequency tables. 
#' 
#' @param age A numeric variable representing the age of the participants.
#' @param gender A numeric or factor variable representing the gender of the participants.
#' @param edu A numeric or factor variable representing the educational level of the participants.
#' @param gender_labels If gender is of class "numeric", you can provide a vector with factor labels. 
#' @param edu_labels If edu is of class "numeric", you can provide a vector with factor labels. 
#' @param frequency Logical value indicating whether frequencies should be returned. 
#' @param percent Logical value indicating whether percentages should be returned. 
#' @return The table summarizing the socio-demographic characteristics of a data frame.
#' @examples 
#' # Creating variables
#' age <- c(20, 21, 25, 35, 19, 20)
#' gender <- c(rep("female", 3), rep("male", 3))
#' edu <- c(rep("highschool", 2), rep("middleschool", 4))
#' 
#' # Summarizing
#' (table_1 <- socdem_table(age, gender, edu))
#' 
#' # Rmarkdown table
#' papaja::apa_table(
#'   table_1,
#'   caption = "Socio-Demographics",
#'   stub_indents = list("Gender" = c(2,3),
#'                       "Education" = c(4,5))
#' )
#' @export
socdem_table <- function(age, 
                         gender, 
                         edu,
                         gender_labels = NULL,
                         edu_labels = NULL,
                         frequency = TRUE,
                         percent = TRUE) {
  
  # dependencies
  library(tidyverse)
  
  tab_age <- describe_vars(age,
                           var_name = "Age (years)") %>%
    select(variable, mean, sd) %>%
    mutate(mean = printnum(mean),
           sd = printnum(sd)) %>%
    unite(percent, c("mean", "sd"), sep = " (") %>%
    mutate(percent = paste0(percent, ")"))
  
  
  tab_gen <- describe_factor(gender, 
                             labels = gender_labels,
                             col_sum = F) %>%
    set_colnames(c("variable", "frequency", "percent")) %>%
    mutate(percent = printnum(percent))
  
  tab_edu <- describe_factor(edu, 
                             labels = edu_labels,
                             col_sum = F) %>%
    set_colnames(c("variable", "frequency", "percent")) %>%
    mutate(percent = printnum(percent))
  
  temp <- bind_rows(tab_age, tab_gen, tab_edu) %>%
    select(variable, frequency, percent) %>%
    mutate(frequency = as.character(frequency))
  
  temp[is.na(temp)] <- ""
  
  if (!isTRUE(frequency)) {
    temp <- temp %>%
      select(-frequency)
  }
  
  if (!isTRUE(percent)) {
    temp <- temp %>%
      select(-percent)
  }
  
  return(temp)
  
}
