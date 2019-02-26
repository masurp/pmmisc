#' Invert 5- or 7-point scales
#' 
#' This function serves as a wrapper for \code{recode} from the "car" package. It allows to easily invert 5- or 7-point scales. 
#' 
#' @param x The variables that should be inverted
#' @param length Is it a 5-point of 7-point scale?
#' @return A vector
#' @export
invert <- function(x, 
                   length = 5){
  
  # dependencies
  if(!require(car)){install.packages('car')}
  library(car)
  
  if(length == 5) {
    car::recode(x, "1=5;2=4;3=3;4=2;5=1;NA=NA")
  } else if(length == 7) {
    car::recode(x, "1=7;2=6;3=5;4=4;5=3;6=2,7=1;NA=NA")
  }
}
