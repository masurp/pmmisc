#' Invert scales with any number of answer options
#' 
#' This function allows to easily invert items with any number of answer options. 
#' 
#' @param x The variable that should be inverted (needs to be numeric!).
#' @param length Number of answer options (defaults to )
#' @return A vector representing the recoded variable. 
#' @export
invert <- function(x, 
                   length = 5) {
  (x-(length+1))*-1
}
