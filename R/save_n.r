#' Save numer of cases as numeric value
#' 
#' Simply a convenient function to save the number of cases of a data frame. This function primarily serves to provide values for the wrapper function \code{\link[pmmisc]{sample_sizes}}.
#' 
#' @param data A data frame.
#' @return A numeric value.
#' @examples
#' d <- mtcars
#' save_n(d)
#' save_n(d[1:4,])
#' 
#' # In combination with the function "sample_sizes"
#' sample_sizes(save_n(d),
#'              save_n(d[1:4,]),
#'              sample = c("Sample 1", "Sample 2", 
#'              print = TRUE))
#' @export
save_n <- function(data) {
  as.numeric(nrow(data))
}




