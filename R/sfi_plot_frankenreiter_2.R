#' Frankenreiter 2
#' 
#' Generate a plot for Frankenreiter figure 2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr

sfi_plot_frankenreiter_2 <- function(){
  
  # Get data
  data <- all_data$frankenreiter$f2
  data <- data %>%
    tidyr::gather(key = key,
           value = value,
           number_tokens: mean_sent_length)
  
  # Relabel
  key_dict <- 
    data.frame(key = c("ave_syllabels", 
                       "mean_sent_length",
                       "number_tokens", 
                       "perc_differentwords2"),
               new_key = c('Mean syllables per word',
                           'Mean sentence length',
                           'Tokens per opinion',
                           'Type-token ratio'))
  data <- left_join(data, key_dict, by= 'key')
  
  
  date_breaks <- as.Date(paste0(seq(1950, 
                                    2010,
                                    by = 10),
                                '-01-01'))
  date_labels <- as.character(seq(1950, 
                                  2010,
                                  by = 10))
  
  g1 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_point(size = 1,
               alpha = 1,
               color = 'black',
               pch = '.') +
    geom_smooth(se = TRUE,
                fill = 'darkgrey',
                alpha = 1,
                linetype = 0) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Date',
         y = '',
         title = 'Figure 1',
         subtitle = 'Development over time of four measures of style over time. Each opinion is represented by one point per panel',
         caption = '*Lines smoothed using local regression') 
  
  
  
  x <- data %>%
    mutate(year = as.numeric(format(docdate, '%Y'))) %>%
    mutate(val = value) %>%
    group_by(year, new_key) %>%
    summarise(value = mean(value, na.rm = TRUE),
              p25 = quantile(val, na.rm = TRUE, 0.25),
              p75 = quantile(val, na.rm = TRUE, 0.75)) %>%
    ungroup %>%
    mutate(year = as.Date(paste0(year, '-01-01')))
  g2 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.6) +
    geom_point(size = 2,
               alpha = 1) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Date',
         y = '',
         title = 'Figure 1 (Version 2)',
         subtitle = 'Development over time of four measures of style over time. Each opinion is represented by one point per panel',
         caption = '*Std error smoothed using local regression. Showing only mean points.') 

  
  
  return(list(g1,g2))
}
