#' Dumas 4
#'
#' Generate a plot for Dumas Figure 4
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr


sfi_plot_dumas_4 <- function() {
  
  # Get data
  data <- all_data$dumas$f4
  
  
  # create a variable to represent the colors 
  data$run <- ifelse(grepl('1', data$label), 'Run 1',
                     ifelse(grepl('2', data$label), 'Run 2',
                            'Run 3'))
  
  data$filter <- ifelse(grepl('with embeddings', data$label), 
                        'Permissive embeddings', 'Permisive')
  
  
  ## --------------------------------------------------
  # g1
  # facet wrap
  g1 <- 
    ggplot(data, 
           aes(x = `True negative rate`,
               y =`True positive rate`,
               color = filter)) +
    geom_point(size = 1,
               alpha = 0.6) +
    geom_line(size = 0.5, 
              alpha = 0.7) +
    scale_color_manual(name = 'Filter type',
                       values = c('black', 'grey')) +
    labs(title = 'Figure 4',
         caption = paste0('The ROC curves of the language model')) +
    geom_abline(intercept = 1,
                color = 'black',
                linetype = 'dashed',
                slope = -1,
                size = 1,
                alpha = 0.6) +
    facet_wrap(~run, ncol = 3) +
    theme_sfi(lp = 'bottom',
              x_axis_title_style = 'bold',
              y_axis_title_style = 'bold') 

  
  
  out <- list(g1)
  return(out)
}


