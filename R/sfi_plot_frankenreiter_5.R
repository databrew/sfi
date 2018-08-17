#' Frankenreiter 5
#' 
#' Generate a plot for Frankenreiter figure 5
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges
#' @import ggrepel

sfi_plot_frankenreiter_5 <- function(){
  
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
  
  panel_groups <- sort(unique(data$panel_group))
  
  g1 <- ggplot(data = data %>% filter(panel_group == panel_groups[1]),
         aes(x = coord2,
             y = coord1)) +
    geom_point(alpha = 0.6) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 1, ', panel_groups[1]))
  
  g1b <- ggplot(data = data %>% filter(panel_group == panel_groups[2]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.6) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 1, ', panel_groups[2]))
  
  g2 <- ggplot(data = data %>% filter(panel_group == panel_groups[1]),
               aes(x = coord2,
                   y = coord1,
                   size = size)) +
    geom_point(alpha = 0.4) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 2, ', panel_groups[1])) +
    scale_size_continuous(name = '',
                          range = c(1, 4)) +
    theme(legend.position = 'none')
  
  g2b <- ggplot(data = data %>% filter(panel_group == panel_groups[2]),
               aes(x = coord2,
                   y = coord1,
                   size = size)) +
    geom_point(alpha = 0.4) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 2, ', panel_groups[2])) +
    scale_size_continuous(name = '',
                          range = c(1, 4)) +
    theme(legend.position = 'none')
  
  
  g3 <- ggplot(data = data %>% filter(panel_group == panel_groups[1]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size)) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 3, ', panel_groups[1])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[1]),
               aes(label = year),
               alpha = 0.6,
               size = 2)
  
  g3b <- ggplot(data = data %>% filter(panel_group == panel_groups[2]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size)) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 3, ', panel_groups[2])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[2]),
                    aes(label = year),
                    alpha = 0.6,
                    size = 2)
  
  g4 <- ggplot(data = data %>% filter(panel_group == panel_groups[1]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size),
               pch = 1) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 4, ', panel_groups[1])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[1]),
                    aes(label = year),
                    alpha = 0.6,
                    size = 2)
  
  g4b <- ggplot(data = data %>% filter(panel_group == panel_groups[2]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size),
               pch = 1) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 4, ', panel_groups[2])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[2]),
                    aes(label = year),
                    alpha = 0.6,
                    size = 2)
  
  
  g5 <- ggplot(data = data %>% filter(panel_group == panel_groups[1]),
               aes(x = coord2,
                   y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size),
               pch = 3) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 4, ', panel_groups[1])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[1]),
                    aes(label = year),
                    alpha = 0.6,
                    size = 2)
  
  g5b <- ggplot(data = data %>% filter(panel_group == panel_groups[2]),
                aes(x = coord2,
                    y = coord1)) +
    geom_point(alpha = 0.7,
               aes(size = size),
               pch = 3) +
    theme_sfi() +
    labs(x = 'Coordinate 2',
         y = 'Coordinate 1',
         title = paste0('Version 4, ', panel_groups[2])) +
    scale_size_continuous(name = '',
                          range = c(1, 2)) +
    theme(legend.position = 'none') +
    geom_text_repel(data = data %>% filter(label,
                                           panel_group == panel_groups[2]),
                    aes(label = year),
                    alpha = 0.6,
                    size = 2)
  
  l <- list(g1, g1b,
            g2, g2b,
            g3, g3b,
            g4, g4b,
            g5, g5b)
  return(l)
}
