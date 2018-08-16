#' Frankenreiter 6
#' 
#' Generate a plot for Frankenreiter figure 5
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges
#' @import ggrepel
#' @import directlabels


sfi_plot_frankenreiter_6 <- function(){
  
  # Get data
  data <- all_data$frankenreiter$f6 %>%
    mutate(year = as.numeric(year))
  
  # separate data into two datasets based on key
  data_1 <- data[data$key == '1973 enlargement',]
  data_2 <- data[data$key == '1995 enlargement',]
  
  # capitalize data_1 
  data_1$judgetrad <- Hmisc::capitalize(data_1$judgetrad)
  
  # Version 1 with lines at end
  g1 <- ggplot(data = data_1,
               aes(x = year,
                   y = kl.dist3,
                   group = interaction(judgetrad,judge),
                   color = judgetrad)) +
    geom_line(size = 1) +
    xlim(c(1973, 1990)) +
    geom_dl(aes(label = judge), 
            method = list(dl.combine("last.points"), hjust = -0.5, 
                          cex = 0.6,
                          dl.move('DK1', vjust = -1)), alpha = 0.8, color = 'black') +
    scale_color_manual(name = '',
                       values = c('black', '#414141', '#979797', '#C6C6C6')) +
    labs(x = 'Year',
         y = '',
         title = 'Version 1 (1973 enlargement)') +
    theme_sfi(lp = 'right') 
  
  
  
  g1
  
  # version 2 put legend in plot
  g2 <- ggplot(data = data_1,
               aes(x = year,
                   y = kl.dist3,
                   group = judge,
                   linetype = judgetrad)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_dl(aes(label = judge), 
            method = list(dl.combine("last.points"), hjust = -0.5, 
                          cex = 0.6,
                          dl.move('DK1', vjust = -1)), alpha = 0.8, color = 'black') +
    xlim(c(1973, 1990)) +
    scale_linetype_manual(name = '',
                      values = c('solid', 'twodash', '1f', 'longdash'))+
    labs(x = 'Year',
         y = '',
         title = 'Version 2 (1973 enlargement') + 
    theme_sfi(lp = c(0.8, 0.2), 
              lj = c(0.8, 0.2), 
              lkh = TRUE,
              lkw = TRUE,
              lba = TRUE,
              legend_height =0.8,
              legend_width = 1.5,
              lkt = 'lines',
              lfb = 'bold',
              la = 0.01)
  
  g2
  
  
  # Version 3 use points
  g3 <- ggplot(data = data_1,
               aes(x = year,
                   y = kl.dist3,
                   group = judge,
                   shape = judgetrad)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(alpha = 0.5) +
    
    geom_dl(aes(label = judge), 
            method = list(dl.combine("last.points"), hjust = -0.5, 
                          cex = 0.6,
                          dl.move('DK1', vjust = -1)), alpha = 0.8, color = 'black') +
    xlim(c(1973, 1990)) +
    scale_shape_manual(name = '',
                          values = c(0, 1, 6, 15))+
    labs(x = 'Year',
         y = '',
         title = 'Version 3 (1973 enlargement') + 
    theme_sfi(lp = c(0.8, 0.2), 
              lj = c(0.8, 0.2), 
              lkh = TRUE,
              lkw = TRUE,
              lba = TRUE,
              legend_height =0.8,
              legend_width = 1.5,
              lkt = 'lines',
              lfb = 'bold',
              la = 0.01)
  
  g3
  
  # version 4 facet wrap with judgetrad
  g4 <- ggplot(data = data_1,
               aes(x = year,
                   y = kl.dist3,
                   group = judge,
                   color = judge)) +
    geom_point(alpha = 0.4) +
    geom_line(alpha = 0.9) +
    xlim(c(1973, 1990)) +
    geom_dl(aes(label = judge), 
           method = list(dl.combine("last.points"), hjust = -0.5, 
                         cex = 0.6,
                         dl.move('DK1', vjust = -1)), alpha = 0.8, color = 'black') +
    scale_color_manual(name = '',
                       values = c('#0D0D0D', '#0D0D0D', '#0D0D0D', '#0D0D0D', '#0D0D0D', '#0D0D0D')) +
    facet_wrap(~judgetrad) +
    labs(x = 'Year',
         y = '',
         title = 'Version 4 (1973 enlargement)') +
    theme_sfi(lp = 'none') 
  g4
  
  
  # version 5 barplot
  
    
  return(list(g1,g2,g3,g4,g5,g6,g7,g8))
}

