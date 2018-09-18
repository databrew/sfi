#' Chen 3
#' 
#' Generate a plot for chen Figure 3
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import Hmisc


sfi_plot_chen_3 <- function(){
  
  # # no scientific notation
  # options(scipen = '999')
  # 
  # # get data 
  data <- all_data$chen$f3
  names(data) <- gsub('-', '.', names(data), fixed = TRUE)
  
  data <- data[data$big.issue != 'Others',]
  data <- data[data$big.issue != '9',]
  data$big.issue <- as.factor(data$big.issue)
  JY_levels <- levels(data$big.issue)
  JY_labels <- c('1 - Criminal Appeal', '2 - Civil Rights', '3 - First Amendment','4 - Due Process',
                 '5 - Privacy', '6 - Labor','7 - Regulation')
  JY_labels_n <- c('1', '2', '3','4','5', '6','7')
  data$big.issue.n <- data$big.issue
  data$big.issue  <- factor(data$big.issue, levels=JY_levels, labels=JY_labels)
  
  levels(data$big.issue.n)
  levels(data$big.issue)
  
  data$big.issue.n  <- factor(data$big.issue.n, levels=JY_levels, labels=JY_labels_n)
  
  # # remove 9 and others
  # data <- data %>% filter(`big-issue` != '9') %>%
  #   filter(`big-issue` != 'Others')
  # 
  # Y_labels <- c('1 - Criminal Appeal', '2 - Civil Rights', '3 - First Amendment','4 - Due Process',
  #               '5 - Privacy', '6 - Labor','7 - Regulation','9 - Miscellaneous', 'Others')
  # JY_labels_n <- c('1', '2', '3','4','5', '6','7','9', 'Others')
  # 
  # 
  # version 11
  g1 <- ggplot(data,
               aes(x = x,
                   y = y)) +
    geom_text(aes(label = `big.issue.n`),
               size = 2,
               color = 'black',
               alpha = 0.4) +
    stat_ellipse(aes(x, y,color= `big.issue.n`),type = "norm", size = 0.3, linetype = 'dashed', alpha = 0.7) +
    scale_color_manual(name = 'Confidence elipse',
                       values = c('#0D0D0D','#0D0D0D', '#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D',
                                  '#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D','#0D0D0D')) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 3.',
         subtitle = ': Centered by Judge-Year, Averaged by Topic-Year, Labeled by Topic',
         caption = paste0('Big Topics and Year, SC and CC Topic Year Vector, Demeaned by Judge & Year', 
                          '\n',
                          '*Ellipses represent a confidence measurement for the clusters (Version 1)')) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold') +
  annotate(geom = 'text',
           x = 0,
           y = -50,
           label = 'Regulation') + annotate("text", x = 0, y = -45, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 0,
             y = 50,
             label = 'Criminal Appeal') +
    annotate(geom = 'text',
             x = -12,
             y = -27,
             label = '1st Amendment') + annotate("text", x = -12, y = -24, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = -40,
             y = 28,
             label = 'Civil Rights') + 
    annotate(geom = 'text',
             x = 17,
             y = 31,
             label = 'Privacy') + annotate("text", x = -12, y = -24, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 38,
             y = 13,
             label = 'Labor') + annotate("text", x = 38, y = 17, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 34,
             y = 0,
             label = 'Due Process') 
    
  
  g2 <- ggplot(data,
               aes(x = x,
                   y = y)) +
    geom_text(aes(label = `big.issue.n`),
              size = 2,
              color = 'black',
              alpha = 0.4) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 3.',
         subtitle = ': Centered by Judge-Year, Averaged by Topic-Year, Labeled by Topic',
         caption = paste0('Big Topics and Year, SC and CC Topic Year Vector, Demeaned by Judge & Year', 
                          '\n',
                          '*Ellipses represent a confidence measurement for the clusters (Version 1)')) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold') +
    annotate(geom = 'text',
             x = 0,
             y = -50,
             label = 'Regulation') + annotate("text", x = 0, y = -45, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 0,
             y = 50,
             label = 'Criminal Appeal') +
    annotate(geom = 'text',
             x = -12,
             y = -27,
             label = '1st Amendment') + annotate("text", x = -12, y = -24, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = -40,
             y = 28,
             label = 'Civil Rights') + 
    annotate(geom = 'text',
             x = 17,
             y = 31,
             label = 'Privacy') + annotate("text", x = -12, y = -24, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 38,
             y = 13,
             label = 'Labor') + annotate("text", x = 38, y = 17, label = sprintf('\u2191')) +
    annotate(geom = 'text',
             x = 34,
             y = 0,
             label = 'Due Process') 
  

  g3 <- ggplot(data,
               aes(x = x,
                   y = y,
                   color = big.issue,
                   label = big.issue.n)) +
    geom_text(size = 3,
              alpha = 0.5) +
    geom_point(size = 0,
              pch = 16,
              alpha = 0) +
    scale_color_manual(name = '',
                       values = c('black', 'black', 'black', 'black', 'black', 'black', 'black')) +
    guides(label = guide_legend(override.aes = list(text = 0)),
           color = guide_legend(override.aes = list(text = 0))) +
    labs(x = 'X',
         y = 'Y',
         title = 'Figure 3.',
         subtitle = ': Centered by Judge-Year, Averaged by Topic-Year, Labeled by Topic',
         caption = paste0('Big Topics and Year, SC and CC Topic Year Vector, Demeaned by Judge & Year', 
                          '\n',
                          '*Ellipses represent a confidence measurement for the clusters (Version 1)')) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              title_style = 'bold') 
  

  
  return(list(g1, g2, g3))
}
