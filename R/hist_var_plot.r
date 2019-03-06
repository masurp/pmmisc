#' Create histograms for several Likert-type variables
#' 
#' This functions computes a facetted plot of variable distributions. Using the density function, the first layer is a histogram and the second a normal distributed density curve based on the mean and standard deviation of the respective variable.  
#' 
#' @param data An object of one of the following classes: "numeric", "double", "integer", "data.frame", "tbl", or "tbl_df".
#' @param bins Number of values that should be plotted on the x-axis (should equal the number of answer options or a reasonable scale).
#' @param fill Color of the bins (defaults to green). 
#' @param color Color of the bin borders (defaults to white).
#' @param density A logical value indicating whether a normally distributed density curve should be plotted on top of the histogram. The density curve takes the mean and sd of the respective variables as arguments. In this case, the histograms are computed using \code{aes(y = stat(density)}. 
#' @param time If the data is panel data, this argument can be used to provide the time variable. Yet, in this case, the data has to be transformed into the long format. This argument only works in combination with the following argument "key" (see examples).
#' @param key If the data is panel data, this argument can be used to provide the key variable (variable that indicates the variable names in the long format). Again, in this case, the data has to be transformed into the long format. This argument only works in combination with the following argument "time" (see examples).
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further customized using standard ggplot2 elements.
#' @examples
#' ## Example 1: Standard usage
#' 
#' d <- data.frame(x = rnorm(100, 3, 1),
#'                 y = rnorm(100, 3, 1))
#'
#' # Several variables
#' hist_var_plot(d, density = F)
#' hist_var_plot(d)
#'
#' # One variables
#' hist_var_plot(d$x)
#'
#' ## Example 2: Panel data
#'
#' d <- data.frame(T1_var1 = rnorm(100, 3, 1),
#'                 T2_var1 = rnorm(100, 3.5, 1),
#'                 T1_var2 = rnorm(100, 2, 1),
#'                 T2_var2 = rnorm(100, 1.5, 1))
#'
#' # Transforming data into long format
#' d %>% 
#'   gather(key, value) %>%
#'   separate(key, c("time", "key"), sep = 2) %>%
#'   hist_var_plot(., time = time, key = code)
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
  
  # get class
  class_data <- class(data)
  if (length(class_data) != 1) {
    class_data <- class_data[1]
    }
  
  ### If data is a single numeric variable
  if ((is.element("integer", class_data) | 
      is.element("double", class_data) | 
      is.element("numeric", class_data)) &
      is.null(time)) {
    
    
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
  } else if (is.element("data.frame", class_data) &
             is.null(time)) {

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
        
        # histogram with normal distribution density curve
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
      
    } else if (!is.null(time)) {
      
      if (density == FALSE) {
      
      # Only histogram
      plot <- ggplot(data,
                     aes(x = value)) + 
        geom_histogram(bins = bins,
                       fill = fill,
                       color = color) +
        facet_grid(key~time) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
      
    } else {
      
      # histogram with normal distribution density curve
      plot <- ggplot(data, 
                     aes(x = value)) + 
        geom_histogram(aes(y = stat(density)), 
                       bins = bins,
                       fill = fill,
                       color = color) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(data$value),
                                  sd = sd(data$value)), 
                      lwd = .5, 
                      col = "black") +
        facet_grid(key~time) +
        labs(x = "Value",
             y = "Density")
      
      return(plot)
      
    }
    
    } else {
    
    message("You need to provide a data.frame or a numeric vector to run this function!")
    
 }
}
