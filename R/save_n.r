#' Save numer of cases as numeric value
#' 
#' Just a convenient function to save the number of cases of a data frame. This function primarily serves to provide values for the wrapper \code{sample_sizes()} in this package.
#' 
#' @param data A data frame.
#' @return A numeric value.
#' @examples
#' d <- mtcars
#' save_n(d)
#' @export
save_n <- function(data) {
  as.numeric(nrow(data))
}




