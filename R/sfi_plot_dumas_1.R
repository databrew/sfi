#' Dumas 1
#'
#' Generate a plot for Dumas Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_dumas_1 <- function() {
  
  # Get data
  data <- all_data$dumas$f1
  
  # --------------------------------------------------
  # g1
  # plot with 2 colors, 3 line types
  
  # create a variable to represent the colors 
  data$run <- ifelse(grepl('1', data$label), 'Run 1',
                     ifelse(grepl('2', data$label), 'Run 2',
                            'Run 3'))
  
  data$filter <- ifelse(grepl('Restrictive', data$label), 
                        'Restrictive', 'Permissive')
  
  #g1 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               col = filter,
               linetype = run)) +
    geom_line(size = 0.5, 
              alpha = 0.7) +
    scale_linetype_manual(name = 'Resampled iterations',
                          values=c("solid", 
                                   "dashed", 
                                   "dotted")) +
    scale_color_manual(name = 'Filter type',
                       values = c('black', 'grey')) +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 1,
                alpha = 0.2) +
    theme_sfi() 
  
    
    # --------------------------------------------------
    # g2
    # plot with 3 colors, 2 line types
    #g2 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               col = run,
               linetype = filter)) +
      geom_line(size = 0.5, 
                alpha = 0.7) +
      scale_linetype_manual(name = 'Resampled iterations',
                            values=c("solid", 
                                     "dotted")) +
      scale_color_manual(name = 'Filter type',
                         values = c('black', 
                                    '#464646', 
                                    '#BEBEBE')) +
      geom_abline(intercept = 1, 
                  slope = -1,
                  size = 1,
                  alpha = 0.2) +
      theme_sfi() 
    
    
    # remove 45 degree line
    #g1 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               col = filter,
               linetype = run)) +
      geom_line(size = 0.5, 
                alpha = 0.7) +
      scale_linetype_manual(name = 'Resampled iterations',
                            values=c("solid", 
                                     "dashed", 
                                     "dotted")) +
      scale_color_manual(name = 'Filter type',
                         values = make_colors(3, 
                                              bw=T)) +
      theme_sfi() 
    
  
  
  # usge geom_point and geom_line
  
  # out <- list(g1, g2, g3, g4)
  return(out)
  
}


