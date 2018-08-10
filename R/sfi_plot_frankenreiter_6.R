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

sfi_plot_frankenreiter_6 <- function(){
  
  # Get data
  data <- all_data$frankenreiter$f6 %>%
    mutate(year = as.numeric(year))
  
  g1 <- ggplot(data = data,
         aes(x = year,
             y = kl.dist3,
             group = judge,
             lty = judge)) +
    geom_line() +
    facet_wrap(~key,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 1') +
    theme_sfi() +
    guides(lty = guide_legend(ncol = 2,
                              title = ''))
  
  g2 <- ggplot(data = data,
               aes(x = year,
                   y = kl.dist3,
                   group = judge,
                   lty = judge)) +
    geom_line() +
    facet_wrap(~key,
               scales = 'free',
               ncol = 1) +
    labs(x = 'Year',
         y = '',
         title = 'Version 2') +
    theme_sfi() +
    guides(lty = guide_legend(ncol = 2,
                              title = ''))
  
  g3 <- ggplot(data = data,
               aes(x = year,
                   y = kl.dist3,
                   group = judge,
                   lty = judge)) +
    geom_line() +
    facet_wrap(~key,
               scales = 'free',
               ncol = 1) +
    labs(x = 'Year',
         y = '',
         title = 'Version 3') +
    theme_sfi() +
    guides(lty = guide_legend(ncol = 10,
                              title = '')) +
    theme(legend.position = 'bottom')
  
  g4 <- ggplot(data = data %>%
                 mutate(judgetrad = Hmisc::capitalize(judgetrad)) %>%
                 distinct(judgetrad, year, key, .keep_all = T),
               aes(x = year,
                   y = kl.dist3)) +
    geom_line() +
    facet_grid(judgetrad~key,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 4') +
    theme_sfi() 
  
  g5 <- ggplot(data = data %>%
                 mutate(judgetrad = Hmisc::capitalize(judgetrad)) %>%
                 distinct(judgetrad, year, key, .keep_all = T),
               aes(x = year,
                   y = kl.dist3)) +
    geom_area(alpha = 0.5,
              color = 'black') +
    facet_grid(judgetrad~key,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 5') +
    theme_sfi() 
  
  g6 <- ggplot(data = data %>%
                 mutate(judgetrad = Hmisc::capitalize(judgetrad)) %>%
                 distinct(judgetrad, year, key, .keep_all = T),
               aes(x = year,
                   y = kl.dist3)) +
    geom_point(alpha = 0.6) +
    geom_line(alpha = 0.6) +
    facet_grid(key ~ judgetrad,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 6') +
    theme_sfi() 
  
  g7 <- ggplot(data = data %>%
                 mutate(judgetrad = Hmisc::capitalize(judgetrad)) %>%
                 distinct(judgetrad, year, key, .keep_all = T),
               aes(x = year,
                   y = kl.dist3)) +
    geom_area(alpha = 0.5,
              color = 'black') +
    facet_grid(key~judgetrad,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 7') +
    theme_sfi()
  
  g8 <- ggplot(data = data %>%
                 mutate(judgetrad = Hmisc::capitalize(judgetrad)) %>%
                 distinct(judgetrad, year, .keep_all = T),
               aes(x = year,
                   y = kl.dist3)) +
    geom_area(alpha = 0.5,
              color = 'black') +
    facet_wrap(~judgetrad,
               scales = 'free') +
    labs(x = 'Year',
         y = '',
         title = 'Version 8') +
    theme_sfi()
    
  return(list(g1,g2,g3,g4,g5,g6,g7,g8))
}
