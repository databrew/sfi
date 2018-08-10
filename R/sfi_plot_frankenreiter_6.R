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
  data <- all_data$frankenreiter$f5_1
  data2 <- all_data$frankenreiter$f5_2
  data <- 
    bind_rows(
      data %>% mutate(panel = 1),
      data2 %>% mutate(panel = 2)
    ) %>%
    dplyr::select(-x1)
  
  label_years <- c(1955, 1960, 1965, 1970, 1973, 1981, 1986,
                   1990, 1995, 2000, 2004, 2007, 2010, 2014)
  data$label <- data$year %in% label_years
  data$panel <- ifelse(data$panel == 1,
                       'Metrix MDS (1955-2014)',
                       'Metrix MDS (1970-2014)')
  
  data$size <- sample(1:5, nrow(data), replace = TRUE)
  data$panel_group <- ifelse(data$year <= 1969, '1950-1969', '1970-2014')
  
  g1 <- ggplot(data = data,
         aes(x = coord2,
             y = coord1)) +
    geom_point(alpha = 0.6) +
    facet_wrap(~panel) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 1')
  g2 <- ggplot(data = data,
               aes(x = coord2,
                   y = coord1,
                   size = size)) +
    geom_point(alpha = 0.4) +
    facet_wrap(~panel) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 2') +
    scale_size_continuous(name = '',
                          range = c(1, 4)) +
    theme(legend.position = 'none')
  
  
  g3 <- ggplot(data = data,
               aes(x = coord2,
                   y = coord1,
                   size = size)) +
    geom_point(alpha = 0.7,
               pch = 1) +
    facet_wrap(~panel) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 3') +
    scale_size_continuous(name = '',
                          range = c(1, 4)) +
    theme(legend.position = 'none')
  
  g4 <- ggplot(data = data %>%
                 arrange(year) %>%
                 filter(!duplicated(year)),
               aes(x = coord2,
                   y = coord1,
                   size = size,
                   pch = panel_group)) +
    geom_point(alpha = 0.7) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 4') +
    scale_size_continuous(name = '',
                          range = c(1, 4)) +
    scale_shape_manual(name = 'Year',
                       values = c(16, 17))
  
  g5 <- ggplot(data = data,
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size)) +
    facet_wrap(~panel) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 5') +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label),
               aes(label = year),
               alpha = 0.6,
               size = 2)
  
  g6 <- ggplot(data = data,
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size)) +
    facet_wrap(~panel) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = 'Version 6') +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_label_repel(data = data %>% filter(label),
                    aes(label = year),
                    alpha = 0.8,
                    size = 2)
  
  l <- list(g1,g2, g3, g4, g5, g6)
  return(l)
}
