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
  
  # create a variable to represent the colors 
  data$col <- ifelse(grepl('1', data$label), 'run_1',
                     ifelse(grepl('2', data$label), 'run_2',
                            'run_3'))
  
  data$line_type <- ifelse(grepl('Restrictive', data$label), 
                           'line_1', 'line_2')
  
  # plot the true negative (x) against true positve (y)
  ggplot(data, 
         aes(`True negative rate`,
             `True positive rate`, 
             color = col,
             linetype = line_type,
             group = interaction(line_type, col))) +
    geom_line(size = 1,
              alpha = 0.6) +
    scale_color_manual(name = '',
                       values = make_colors(3, bw = TRUE))
    theme_sfi() 
  

  # plot the true negative (x) against true positve (y)
  ggplot(data, 
         aes(`True negative rate`,
             `True positive rate`, 
             group = label,
             col = label)) +
    geom_line(size = 1,
              alpha = 0.6) +
    theme_sfi() +
    scale_color_manual(name = '',
                       values = make_colors(length_labels, bw = TRUE)) 
    
  
  # out <- list(g1, g2, g3, g4)
  return(out)

}


