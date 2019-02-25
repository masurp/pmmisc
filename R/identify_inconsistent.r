#' Identify participants who provided inconsistent answers in panel studies
#' 
#' When conducting a panel study, we often want to know whether the same person answers all the waves. One way of investigating this is to check whether people provided consistent birthdates or gender across several waves. This function allows to check the consistency between up to three variables. It returns a vector with the consistent values. Participants with inconsistent answers receive the number \code{-99} which can in turn be used to subset the overall data set.
#' 
#' @param ... The variables that should be evaluated (up to three).
#' @param na.rm Should people with missings in one of the variables be excluded (i.e., receive the value "-99")?
#' @examples 
#' # Create example data
#' (data <- data.frame(x1 = c(1,2,NA), x2 = c(1,2,3)))
#'
#' # Create new variable with consistent answers
#' data$x <- identify_inconsistent(x1, x2, na.rm = T)
#'
#' # Check
#' data
#'
#' # Subset data based on function's output
#'subset(data, x != -99)
#' @export
identify_inconsistent <- function(...,
                                  na.rm = FALSE){
  # Creates a variable in which all inconsistent person receive the value 9999
  
  # Compute number of missings per person
  var_matrix <- data.frame(...)
  
  if (isTRUE(na.rm)) {
    var_matrix[is.na(var_matrix)] <- -99
  }
  
  count_na <- apply(var_matrix, 1, function(x) sum(is.na(x)))
  
  # Identify persons with inconsistent 
  if (length(var_matrix) == 3) {
  ifelse(count_na == 3,
         NA,
         ifelse((count_na == 2 & !is.na(var_matrix[[1]])) |
                (count_na == 1 & (is.na(var_matrix[[2]]) & var_matrix[[1]] == var_matrix[[3]])) |
                (count_na == 1 & (is.na(var_matrix[[3]]) & var_matrix[[1]] == var_matrix[[2]])),
                var_matrix[[1]],
                ifelse((count_na == 2 & !is.na(var_matrix[[2]]))|
                       (count_na == 1 & is.na(var_matrix[[1]]) & var_matrix[[2]] == var_matrix[[3]]),
                       var_matrix[[2]],
                       ifelse(count_na == 2 & !is.na(var_matrix[[3]]),
                              var_matrix[[3]],
                              ifelse((count_na == 1 & is.na(var_matrix[[1]]) & var_matrix[[2]] != var_matrix[[3]]) |
                                     (count_na == 1 & is.na(var_matrix[[2]]) & var_matrix[[1]] != var_matrix[[3]]) |
                                     (count_na == 1 & is.na(var_matrix[[3]]) & var_matrix[[1]] != var_matrix[[2]]) |
                                     (count_na == 0 & (var_matrix[[1]] != var_matrix[[2]] | var_matrix[[1]] != var_matrix[[3]] | var_matrix[[2]] != var_matrix[[3]])),
                                     -99, var_matrix[[1]])))))
  } else if (length(data) == 2) {
    ifelse(count_na == 2,
           NA,
           ifelse(count_na == 1 & !is.na(var_matrix[[2]]),
                  var_matrix[[2]],
                  ifelse(count_na == 1 & !is.na(var_matrix[[1]]),
                         var_matrix[[1]],
                         ifelse(count_na == 0 & (var_matrix[[1]] != var_matrix[[2]]),
                                -99, var_matrix[[1]]))))
  }
}
