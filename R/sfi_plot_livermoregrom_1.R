#' Livermoregrom 1
#' 
#' Generate a plot for Livermoregrom Figure 1
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc
#' @import reshape2


sfi_plot_livermoregrom_1 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$livermoregrom$f1
  
  # get those columns 
  data <- data[, c('agency_ideol', 'sent_agency', 
                   'ml_mean_compress2', 'agency_ideol_E')]
  
  data$agency_ideol <- as.numeric(data$agency_ideol)
  
  # make long form
  data <- melt(data)
  
  g1 <- ggplot() + geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
                                  data$value[data$variable =='ml_mean_compress2'], 
                                  size = 'Rule Sentiment'),
                              alpha = 0.9) +
    geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
                   data$value[data$variable =='sent_agency'],
                   size = 'Agency Average Summit'), 
               color = '#6D6D6D', alpha = 0.1) +
    xlab('Agency Polarity') + ylab('Sentiment') +
    theme_sfi(lp = 'bottom', x_axis_title_style = 'bold', y_axis_title_style = 'bold') +
    scale_size_manual(name = '',
                      values = c(4,1))
  
  
  return(list(g1))
}
