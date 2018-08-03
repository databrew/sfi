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
  
  # Plot
  g1 <- ggplot(data = data,
         aes(x = docdate,
             y = value)) +
    geom_point(size = 0.3,
               alpha = 0.3,
               pch = 1) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 1,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                     breaks = date_breaks, 
                     labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 1')
  g2 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_point(size = 0.1,
               alpha = 0.3,
               pch = '.') +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 2') + 
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  x <- data %>%
    mutate(year = as.numeric(format(docdate, '%Y'))) %>%
    group_by(year, new_key) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(year = as.Date(paste0(year, '-01-01')))
  
  g3 <- ggplot(data = x,
               aes(x = year,
                   y = value)) +
    geom_line() +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 1,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 3')
  
  g4 <- ggplot(data = x,
               aes(x = year,
                   y = value)) +
    # geom_line(alpha = 0.6) +
    geom_point(alpha = 0.6,
               pch = 1) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 1,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 4') +
    geom_line(stat="smooth",method = "loess",
              alpha = 0.5)
  
  g5 <- ggplot(data = x,
               aes(x = year,
                   y = value)) +
    geom_line(alpha = 0.6,
              lty = 2) +
    # geom_point(alpha = 0.6,
    #            pch = 1) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 1,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 5') +
    geom_line(stat="smooth",method = "loess",
              alpha = 0.5)
  
  g6 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_line() +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 6')
  
  g7 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_line(alpha = 0.5) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 7') +
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  g8 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_point(alpha = 0.5) +
    geom_vline(xintercept = date_breaks,
               alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 8') +
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  g9 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_bar(alpha = 0.5, stat = 'identity',
             width = 100) +
    # geom_vline(xintercept = date_breaks,
    #            alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 9') 
  
  g10 <-
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_area(alpha = 0.6,
              color = 'black') +
    # geom_vline(xintercept = date_breaks,
    #            alpha = 0.3) +
    facet_wrap(~new_key, 
               ncol = 2,
               scales = 'free_y') +
    scale_x_date(name = 'Date', 
                 breaks = date_breaks, 
                 labels = date_labels) +
    theme_sfi() +
    labs(x = 'Date',
         y = '',
         title = 'Version 10') 
  
  

  return(list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10))
}
