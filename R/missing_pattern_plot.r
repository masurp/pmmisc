#' Plotting missingness patterns
#' 
#' This function plots the missingness patterns and if wanted also their their frequencies.
#' 
#' @param data A data frame.
#' @param colors Colors to be used in the plot. 
#' @param frequency A logical value indicating whether the frequency of the missingness pattens should be plotted to
#' @param ratio Vector specifying the size of both plots in comparison to one another.
#' @param nrow Underneath or next to each other?
#' @return A gtable. 
#' @examples
#' d <- mtcars
#' d[4,3] <- NA # Create missing to illustrate function
#' 
#' missing_pattern_plot(d)
#' @export
missing_pattern_plot <- function(data,
                                 colors = c("#2F6FAF", "lightblue"),
                                 frequency = TRUE,
                                 ratio = c(2.5,1),#
                                 nrow = 1){
  
  # dependencies
  library(mice)
  library(tidyverse)
  library(gridExtra)
  
  color_2 <- colors[2]
  
  temp <- data %>% 
    md.pattern %>% 
    as.data.frame %>%
    rownames_to_column("sum") %>%
    select(-V3) %>% 
    as.tibble %>%
    subset(sum != "") %>%
    mutate(sum = as.numeric(sum)) %>%
    mutate(n = 1:nrow(.))
  
   main_plot <- temp %>%
    gather(key, value, -n, -sum) %>%
    ggplot(aes(x = key, 
               y = n)) +
    geom_raster(aes(fill = factor(value, 
                                  labels = c("TRUE", 
                                             "FALSE")))) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(x = "Variables",
         fill = "Missings",
         y = "Missingness patterns") +
    theme(legend.position="right",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
   
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
   
   grid.arrange(main_plot, 
                side_plot,
                widths = ratio,
                nrow = nrow)
   }
}
