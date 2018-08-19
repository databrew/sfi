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
  
  
  # --------------------------------------------------
  # g1
  # plot with 3 colors, 2 line types
  g1 <- 
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
    labs(title = 'Version 1') +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 1,
                alpha = 0.2) +
    theme_sfi() 


  # --------------------------------------------------
  # g2
  # get average of runs
  data <- data[order(data$run),]
  data$run_group <- rep.int(1:2000, 3)
  
  # group by run group and get avg
  new_data <- data %>%
    group_by(filter,run_group) %>%
    summarise(avg_tpr = mean(`True positive rate`),
              avg_tnr = mean(`True negative rate`))
  
  g2 <- 
    ggplot(new_data, 
           aes(x = avg_tpr,
               y = avg_tnr,
               col = filter)) +
    geom_line(size = 0.5,) +
    labs(title = 'Version 2',
         subtitle = 'Average over the 3 resampled runs') +
    scale_color_manual(name = '',
                       values = c('black', 'grey')) +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 0.5,
                alpha = 0.6,
                col = 'black',
                linetype = 'longdash') +
    theme_sfi() 
  
  
  g3 <- 
    ggplot(new_data, 
           aes(x = avg_tpr,
               y = avg_tnr,
               col = filter)) +
    geom_point(size = 2,
               pch = 16,
               alpha = 0.2) +
    geom_line(size = 0.5,,
              alpha = 0.7) +
    labs(title = 'Version 3',
         subtitle = 'Average over the 3 resampled runs') +
    scale_color_manual(name = '',
                       values = c('black', 'grey')) +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 0.5,
                alpha = 0.6,
                col = 'black',
                linetype = 'longdash') +
    theme_sfi() 
  
  
  # --------------------------------------------------
  # g4
  # facet wrap
  g4 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               col = filter)) +
    geom_point(size = 1,
               alpha = 0.6) +
    geom_line(size = 0.5, 
              alpha = 0.7) +
    labs(title = 'Version 4') +
    scale_color_manual(name = 'Filter type',
                       values = c('black', 'grey')) +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 1,
                alpha = 0.2) +
    facet_wrap(~run, nrow = 3) +
    theme_sfi() 

  
  # --------------------------------------------------
  # g5
  # facet wrap
  g5 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               linetype = run)) +
    geom_line(size = 0.5, 
              alpha = 0.7) +
    labs(title = 'Version 5') +
    scale_linetype_manual(name = 'Resampled iterations',
                          values = c("solid", 
                                     "dashed", 
                                     "dotted")) +
    geom_abline(intercept = 1, 
                slope = -1,
                size = 1,
                alpha = 0.2) +
    facet_wrap(~filter, nrow = 2) +
    theme_sfi() 
  
  out <- list(g1, g2, g3, 
              g4, g5)
  return(out)
  
}


