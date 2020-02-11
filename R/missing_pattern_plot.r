#' Missingness pattern plot
#' 
#' This function plots the missingness patterns and (if desired) also their their frequencies in the data frame.
#' 
#' @param data The data frame that should be evaluated.
#' @param colors Colors to be used in the plot. 
#' @param titles Both plots can be named individually. By default, they are simply labelled "A" and "B".
#' @param var_labels A logical value indicating whether variable names should be plotted (if many variables are in the data frame, setting this to TRUE can be messy).
#' @param frequency A logical value indicating whether the frequency of the missingness pattens should be plotted to.
#' @param percent A logical value indicating whether the share of cases that have a particular missingness pattern should be plotted into the frequency table (still beta, doesn't look very nice..., simply helps to understand the data)
#' @param ratio Vector specifying the size of both plots in comparison to one another.
#' @param nrow Should the plots be printed underneath or next to each other? Defaults to "next to each other".
#' @return A \code{gtable} which consists by default of two plots: (A) An overview of all existing missingness patterns in the data frame, (B) a frequency table representing how often the particular missingness patterns can be found in the data frame. 
#' @examples
#' d <- mtcars
#' d[4,3:4] <- NA # Create missing to illustrate function
#' 
#' missing_pattern_plot(d)
#' 
#' # Add variable names and plot only patterns
#' missing_pattern_plot(d, var_labels = T, frequency = T)
#' @export
missing_pattern_plot <- function(data,
                                 colors = c("#2F6FAF", "lightblue"),
                                 titles = c("A", "B"),
                                 var_labels = FALSE,
                                 frequency = TRUE,
                                 percent = FALSE,
                                 ratio = c(2.5,1),
                                 nrow = 1){
  
  # dependencies
  library(mice)
  library(tidyverse)
  library(gridExtra)
  library(cowplot)
  
  # Break message
  if (is.null(data)) {
    message("You need to provide a data frame!")
  }
  
  color_2 <- colors[2]
  
  temp <- data %>% 
    mice::md.pattern(plot = FALSE) %>% 
    as.data.frame %>%
    tibble::rownames_to_column("sum") %>% 
    dplyr::select(-ncol(.)) %>%
    as_tibble %>%
    subset(sum != "") %>%
    dplyr::mutate(sum = as.numeric(sum)) %>%
    dplyr::mutate(n = 1:nrow(.)) %>%
    dplyr::mutate(n = factor(n, levels = n[order(-sum)]))
  
   main_plot <- temp %>%
    gather(key, value, -n, -sum) %>%
    ggplot(.,
           aes(x = key, 
               y = n)) +
    geom_tile(aes(fill = factor(value, 
                                labels = c("TRUE", 
                                           "FALSE"))),
                  color = "white") +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(x = "Variables",
         fill = "Missings",
         y = "Missingness patterns") +
    theme(legend.position="right",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
   
   if (!isTRUE(var_labels)) {
     main_plot <- main_plot +
       theme(axis.text.x = element_blank(),
             axis.ticks.x = element_blank())
   }
   
   if (!isTRUE(frequency)) {
     
     return(main_plot)
     
   } else {
     
   main_plot <- main_plot + 
     theme(legend.position = c(1.275, .8),
           legend.background = element_rect(color = "white", fill = 'white'))
   
   side_plot <- temp %>% 
    ggplot(., aes(x = n, y = sum, width = 1)) + 
    geom_bar(stat = "identity",
             fill = color_2) +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "N. of cases")
   
   if (isTRUE(percent)) {
     side_plot <- side_plot +
       geom_text(aes(label = (sum/nrow(data))*100))
   }
   
   plot_grid(main_plot, side_plot, 
             labels = titles, 
             rel_widths = ratio, 
             align = "h", 
             nrow = nrow)
   }
}

