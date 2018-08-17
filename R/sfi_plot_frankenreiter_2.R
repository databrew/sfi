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
         title = 'Version 1',
         caption = '*Lines smoothed using local regression') + 
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  x <- data %>%
    mutate(year = as.numeric(format(docdate, '%Y'))) %>%
    mutate(val = value) %>%
    group_by(year, new_key) %>%
    summarise(value = mean(value, na.rm = TRUE),
              p25 = quantile(val, na.rm = TRUE, 0.25),
              p75 = quantile(val, na.rm = TRUE, 0.75)) %>%
    ungroup %>%
    mutate(year = as.Date(paste0(year, '-01-01')))
  
  g2 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.6) +
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
         title = 'Version 2',
         caption = '*Lines smoothed using local regression; grey area shows annual interquartile range') + 
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  g3 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.9) +
    geom_point(size = 0.1,
               alpha = 0.7,
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
         title = 'Version 3',
         caption = '*Lines smoothed using local regression; grey area shows annual interquartile range') + 
    geom_line(stat="smooth",method = "auto",
              alpha = 0.8)
  
  g4 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.9) +
    geom_point(size = 0.1,
               alpha = 0.7,
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
         title = 'Version 4',
         caption = '*Grey area shows annual interquartile range') 
  
  g5 <- 
    ggplot(data = x,
           aes(x = year,
               y = value)) +
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.6) +
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
         title = 'Version 5',
         caption = 'Line shows average value per year; grey area shows interquartile range')
  
  g6 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.9) +
    geom_point(size = 0.1,
               alpha = 0.7,
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
         title = 'Version 6',
         caption = '*Grey area shows annual interquartile range; solid line shows annual average') +
    geom_line(data = x,
              aes(x = year,
                  y = value))
  
  g7 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.4,
                color = 'black') +
    geom_point(size = 0.1,
               alpha = 0.7,
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
         title = 'Version 7',
         caption = '*Grey area shows annual interquartile range; dashed line shows annual average') +
    geom_line(data = x,
              aes(x = year,
                  y = value),
              lty = 2)
  
  g8 <- ggplot(data = data,
               aes(x = docdate,
                   y = value)) +
    geom_ribbon(data = x,
                aes(x = year,
                    ymin = p25,
                    ymax = p75),
                alpha = 0.9) +
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
         title = 'Version 8',
         caption = '*Grey area shows annual interquartile range; solid grey line shows annual average') +
    geom_line(data = x,
              aes(x = year,
                  y = value),
              color = 'grey')
  
  return(list(g1,g2,g3,g4,g5,g6,g7,g8))
}
