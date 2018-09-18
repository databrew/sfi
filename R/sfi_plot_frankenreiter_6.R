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
  
  # capitalize data 
  data_1$judgetrad <- Hmisc::capitalize(data_1$judgetrad)
  data_2$judgetrad <- Hmisc::capitalize(data_2$judgetrad)
  
  # recode data so the new judgetrad variable indicates if the judge is new
  data_1$judgetrad <- ifelse(data_1$new == 1, paste0(data_1$judgetrad, ' New'), data_1$judgetrad)
  data_2$judgetrad <- ifelse(data_2$new == 1, paste0(data_2$judgetrad, ' New'), data_2$judgetrad)
  
  # -----------------------
  # 1973 enlargement
  
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
         title = 'Figure 6 (1973 Enlargement)',
         subtitle = 'Development of writing style of ECJ in comparison to the writing of judges
between 1973 and 1975') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') 
  
 
  
  
  # version 5 barplot
  g2 <- ggplot(data = data_1,
         aes(x = year,
             y = kl.dist3,
             fill = judgetrad)) +
    geom_point(size = 0.5, 
               color = 'black',
               alpha = 0.4) +
    geom_line(size = 0.5,
              alpha = 0.8) +
    geom_dl(aes(label = judgetrad), 
            method = list(dl.combine("last.points"), cex = 0.7,
                          vjust = -0.3,alpha = 0.8, color = 'black',
                          dl.trans(x = x - 2.8))) +
    ylim(c(0, .1)) +
    labs(x = 'Year',
         y = '',
         title = 'Figure 6 (1973 Enlargement)',
         subtitle = 'Development of writing style of ECJ in comparison to the writing of judges
between 1973 and 1975') +
    theme_sfi(lp = 'bottom',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    scale_fill_manual(name = '',
                      values = c('black', "#595959", "#9C9C9C",'#D3D0D0' )) +
    facet_wrap(~judge) 

    
    # -----------------------
    # 1973 enlargement
    # capitalize data_1 
    data_2$judgetrad <- Hmisc::capitalize(data_2$judgetrad)
    
#     # Version 1 with lines at end - issue
#     g3 <- ggplot(data = data_2,
#                  aes(x = year,
#                      y = kl.dist3,
#                      group = judge)) +
#       ylim(c(0, .04)) +
#       geom_point(size = 0.5, 
#                  color = 'black',
#                  alpha = 0.4) +
#       geom_line(size = 0.5,
#                 alpha = 0.8) +
#       # geom_dl(aes(label = judge), 
#       #         method = list(direct.label("last.points"), cex = 0.7,alpha = 0.8, color = 'black',
#       #                       dl.move('IE4', vjust = -1))) +
#       geom_text_repel(aes(label = max(judge)),
#                       size = 2,
#                       nudge_x = 3,
#                       segment.color = NA,
#                       show.legend = FALSE) +
#       xlim(c(1994, 2014)) +
#       facet_wrap(~judgetrad) +
#       labs(x = 'Year',
#            y = '',
#            title = 'Figure 6 (1995 Enlargement)',
#            subtitle = 'Development of writing style of ECJ in comparison to the writing of judges
# between 1973 and 1975') +
#       theme_sfi(lp = 'bottom',
#                 y_axis_title_style = 'bold',
#                 x_axis_title_style = 'bold',
#                 title_style = 'bold') 
# 
#     
# 
#     g3
#     
    
    
    g3 <- ggplot(data = data_2,
                 aes(x = year,
                     y = kl.dist3,
                     group = judge)) +
      ylim(c(0, .03)) +
      geom_point(size = 0.5, 
                 color = 'black',
                 alpha = 0.4) +
      geom_line(size = 0.5,
                alpha = 0.8) +
      geom_dl(aes(label = judge),
              method = list(dl.combine("last.points"), 
                            cex = 0.5, 
                            alpha = 0.8, 
                            color = 'black',
                            dl.move('IE4', vjust = -1),
                            dl.move('PT1', vjust = 2, hjust = 16),
                            dl.move('BE4', vjust = 3, hjust = 15),
                            dl.move('GR2', vjust = 1))) +
      xlim(c(1992, 2012)) +
      facet_wrap(~judgetrad, nrow = 3) +
      labs(x = 'Year',
           y = '',
           title = 'Figure 6 (1995 Enlargement)',
           subtitle = 'Development of writing style of ECJ in comparison to the writing of judges
between 1973 and 1975') +
      theme_sfi(lp = 'bottom',
                y_axis_title_style = 'bold',
                x_axis_title_style = 'bold',
                title_style = 'bold') 

    
    
  return(list(g1,g2,g3))
}

