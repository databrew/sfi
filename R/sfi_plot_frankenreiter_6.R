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
         title = 'Version 1 (1973 enlargement)') +
    theme_sfi(lp = 'right') 
  
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
  
  
  
  # version 5 barplot
  g5 <- ggplot(data = data_1,
         aes(x = year,
             y = kl.dist3,
             fill = judgetrad)) +
    geom_bar(stat = 'identity',
             position = 'dodge',
             color = 'grey',
             alpha = 0.9) + 
    labs(x = 'Year', 
         y = 'kl.dist3',
         title = 'Version 5 (1973 enlargement)') +
    scale_fill_manual(name = '',
                      values = c('black', "#595959", "#9C9C9C",'#D3D0D0' )) +
    facet_wrap(~judge) + 
    theme_sfi()
    
    
    
    # -----------------------
    # 1973 enlargement
    # capitalize data_1 
    data_2$judgetrad <- Hmisc::capitalize(data_2$judgetrad)
    
    # Version 1 with lines at end
    g6 <- ggplot(data = data_2,
                 aes(x = year,
                     y = kl.dist3,
                     group = judgetrad)) +
      geom_point(size = 0.5, 
                 color = 'black',
                 alpha = 0.4) +
      geom_line(size = 0.5,
                alpha = 0.8) +
      geom_dl(aes(label = judgetrad), 
              method = list(dl.combine("last.points"), cex = 0.7,
                            vjust = 3, alpha = 0.8, color = 'black',
                            dl.trans(x = x - 2.8))) +
      xlim(c(1994, 2012)) +
      labs(x = 'Year',
           y = '',
           title = 'Version 1 (1995 enlargement)') +
      facet_wrap(~judge, scales = 'free') +
      theme_sfi(lp = 'right', 
                base_size = 10,
                gM = FALSE,
                gm = FALSE) 
    
    
    # Version 2 scaled
    g7 <- ggplot(data = data_2,
                 aes(x = year,
                     y = kl.dist3,
                     group = judgetrad)) +
      geom_point(size = 0.5, 
                 color = 'black',
                 alpha = 0.4) +
      geom_line(size = 0.5,
                alpha = 0.8) +
      geom_dl(aes(label = judgetrad), 
              method = list(dl.combine("last.points"), cex = 0.7,
                            vjust = 0, alpha = 0.8, color = 'black',
                            dl.trans(x = x - 2.8))) +
      coord_cartesian(ylim=c(0, .03)) +
      xlim(c(1994, 2012)) +
      labs(x = 'Year',
           y = '',
           title = 'Version 2 (1995 enlargement)') +
      facet_wrap(~judge, scales = 'free') +
      theme_sfi(lp = 'right', 
                base_size = 10,
                gM = FALSE,
                gm = FALSE) 
    
  
    # version 3 barplot
    g8 <- ggplot(data = data_2,
                 aes(x = year,
                     y = kl.dist3,
                     group = judgetrad)) +
      geom_bar(stat = 'identity',
               position = 'dodge',
               color = 'grey',
               fill = 'black',
               alpha = 0.6) + 
      geom_dl(aes(label = judgetrad), 
              method = list(dl.combine("last.points"), cex = 0.7,
                            vjust = 2, alpha = 0.6, color = 'black',
                            dl.trans(x = x - 2.9))) +
      xlim(c(1994, 2012)) +
      labs(x = 'Year',
           y = '',
           title = 'Version 3 (1995 enlargement)') +
      facet_wrap(~judge, scales = 'free') +
      theme_sfi(lp = 'none', 
                base_size = 10,
                gM = FALSE,
                gm = FALSE) 
    
    
    
    # version 4 same y axis
    g9 <- ggplot(data = data_2,
                 aes(x = year,
                     y = kl.dist3,
                     group = judgetrad)) +
      geom_bar(stat = 'identity',
               position = 'dodge',
               color = 'grey',
               fill = 'black',
               alpha = 0.6) + 
      xlim(c(1994, 2012)) +
      coord_cartesian(ylim=c(0, .03)) +
      geom_dl(aes(label = judgetrad), 
              method = list(dl.combine("last.points"), cex = 0.7,
                            vjust = 0, alpha = 0.6, color = 'black',
                            dl.trans(x = x - 2.9))) +
      labs(x = 'Year',
           y = '',
           title = 'Version 4 (1995 enlargement)') +
      facet_wrap(~judge, scales = 'free_y') +
      theme_sfi(lp = 'none', 
                base_size = 10,
                gM = FALSE,
                gm = FALSE) 
    

    
  return(list(g1,g2,g3,g4,g5,g6,g7, g8, g9))
}

