#' Create histograms for several Likert-type variables
#' 
#' This functions computes a facetted plot of variable distributions using the function \code{\link[ggplot2]{ggplot}}. The first layer is a histogram and the second a normal distributed density curve based on the mean and standard deviation of the respective variable. Both a data frame containing several variables or a single vector can be passed to the function. 
#' 
#' @param data An object of one of the following classes: "numeric", "double", "integer", "data.frame", "tbl", or "tbl_df".
#' @param bins Number of values that should be plotted on the x-axis (should equal the number of answer options or a reasonable scale).
#' @param fill Color of the bins (defaults to green). 
#' @param color Color of the bin borders (defaults to white).
#' @param density A logical value indicating whether a normally distributed density curve should be plotted on top of the histogram. The density curve takes the mean and sd of the respective variables as arguments. In this case, the histograms are computed using \code{aes(y = stat(density)}. 
#' @return A \code{\link[ggplot2]{gg}} object that can be further customized using standard ggplot2 elements (e.g., + theme_minimal(), + ylimit(0,10), ...).
#' @examples
#' d <- data.frame(x = rnorm(200, 3, 1),
#'                 y = rnorm(200, 0, 1))
#'
#' # Several variables
#' hist_var_plot(d, density = F, bins = 10) 
#' hist_var_plot(d, bins = 10)
#'
#' # One variable
#' hist_var_plot(d$x, fill = "lightblue", bins = 10) +
#'    theme_minimal()
#' @export
hist_var_plot <- function(data, 
                          bins = 5, 
                          fill = "#5e999b",
                          color = "white",
                          density = TRUE,
                          time = NULL,
                          key = NULL){
  
  # Dependencies
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plyr)
  
  # get class
  class_data <- class(data)
  if (length(class_data) != 1) {
    class_data <- class_data[1]
    }
  
  ### If data is a single numeric variable
  if (is.element("integer", class_data) | 
      is.element("double", class_data) | 
      is.element("numeric", class_data)) {
    
    
    if (density == FALSE) {
      
      # only histogram
      plot <- ggplot(NULL,
                    aes(x = data)) + 
        geom_histogram(bins = bins,
                       fill = fill,
                       color = color) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
      
    } else {
      
      # histogram with normal distribution density curve
      plot <- ggplot(NULL, 
                     aes(x = data)) + 
        geom_histogram(aes(y = stat(density)), 
                       bins = bins,
                       fill = fill,
                       color = color) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(data),
                                  sd = sd(data)), 
                      lwd = .5, 
                      col = "black") +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
    } 
  
    ## Plotting a data frame with several numeric variables
  } else if (is.element("data.frame", class_data) |
             is.element("tbl_df", class_data) |
             is.element("tbl", class_data)) {

      # step 1: ransform data
      data_long <- data %>% 
        gather(key, value)
      
      # step 2: plot long data
      
      if (density == FALSE) {
        
        # Only histogram
        plot <- ggplot(data_long,
                       aes(x = value)) + 
          geom_histogram(bins = bins,
                         fill = fill,
                         color = color) +
          facet_wrap(~key) +
          labs(x = "Value",
               y = "Density")
        
        return(plot)
        
      } else {
        
        grid <- with(data_long, 
                     seq(min(value, na.rm = TRUE), 
                         max(value, na.rm = TRUE), 
                         length = 100))
        normaldens <- ddply(data_long, "key", function(df) {
          data.frame( 
            value = grid,
            density = dnorm(grid, mean(df$value), sd(df$value))
          )
        })
        
        # histogram with normal distribution density curve
        plot <- ggplot(data_long, 
                       aes(x = value)) + 
          geom_histogram(aes(y = stat(density)), 
                         bins = bins,
                         fill = fill,
                         color = color) +
          geom_line(aes(y = density), data = normaldens, colour = "black") +
          facet_wrap(~key) +
          labs(x = "Value",
               y = "Density")
        
        return(plot)
      }
    
    } else {
    
    message("You need to provide a data.frame or a numeric vector to run this function!")
    
 }
}
