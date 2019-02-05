#' Create histograms for several Likert-type variables
#' 
#' This functions computes a facetted plot of variable distributions. Using the density function, the first layer is a histogram and the second a normal distributed density curve based on the mean and standard deviation of the respective variable.  
#' 
#' @param data A data frame including all variables that should be plotted. 
#' @param bins Number of values that should be plotted on the x-axis (should equal the number of answer options)
#' @param fill Color of the bins. 
#' @param color Color of the bin margins.
#' @param density A logical value indicating whether a normally distributed density curve should be plotted on top of the histogram. The density curve takes the mean and sd of the respective variable as arguments. In this case, the histograms are computed using \code{aes(y = stat(density)}. 
#' @return An object of class \code{tibble} that can be passed to \code{apa_table()}. It contains all sample sizes that were provided as individual numeric values. 
#' @examples
#' d <- data.frame(x = rnorm(100, 3, 1),
#'y = rnorm(100, 3, 1))
#'
#'hist_var_plot(d, density = F)
#'hist_var_plot(d)
#' @export
hist_var_plot <- function(data, 
                          bins = 5, 
                          fill = "#5e999b",
                          color = "white",
                          density = TRUE){
  
  # Dependencies
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  if (class(data) == "data.frame") {
    
    # transform data
    data_long <- data %>% 
      gather(key, value)
    
    # plot data 
    if (density == FALSE) {
      
      plot<- ggplot(data_long,
                    aes(x = value)) + 
        geom_histogram(bins = bins,
                       fill = fill,
                       color = color) +
        facet_wrap(~key) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
      
    } else {
      
      plot <- ggplot(data_long, 
                     aes(x = value)) + 
        geom_histogram(aes(y = stat(density)), 
                       bins = bins,
                       fill = fill,
                       color = color) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(data_long$value),
                                  sd = sd(data_long$value)), 
                      lwd = .5, 
                      col = "black") +
        facet_wrap(~key) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
    }
    
  } else {
    
    if (density == FALSE) {
      
      plot<- ggplot(NULL,
                    aes(x = data)) + 
        geom_histogram(bins = bins,
                       fill = fill,
                       color = color) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
      
    } else {
      
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
  }
}
