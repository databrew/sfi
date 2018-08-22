#' Feldman 1
#' 
#' Generate a plot for Feldman Figure 1 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import scales


sfi_plot_feldman_1 <- function() {
  
  # Get data
  data <- all_data$feldman$f1 
  
  # recode federal
  data$federal <- ifelse(data$federal == '1', 'Federal', 'State')

  
  # # VERSION 1
  # g1 <- 
  #   ggplot(data, 
  #          aes(x=federal,
  #              y=clarity_score)) + 
  #   
  #   geom_violin(alpha = 0.7, 
  #               fill = 'black',
  #               color = 'black',
  #               size = 0.1) +
  #   geom_jitter(width = 0.3, 
  #               alpha = 0.5,
  #               color = '#6C6C6C',
  #               size = 0.8,
  #               pch = 16) +
  #   labs(x = '',
  #        y = 'Clarity Score',
  #        title = 'Version 1, points in front, violin in back',
  #        subtitle = paste0('50% transparency on points, 70% transparent on violin', 
  #                          '\n',
  #                          'Outline of violin is normal')) +
  #   theme_sfi() +
  #   scale_fill_manual(name = '',
  #                     values = make_colors(length(unique(data$federal)),
  #                                          categorical = TRUE, 
  #                                          bw = TRUE))
  # 
  # 
  # # VERSION 1
  # g2 <- 
  #   ggplot(data, 
  #          aes(x=federal,
  #              y=clarity_score)) + 
  #   geom_jitter(width = 0.3, 
  #               alpha = 0.5,
  #               color = 'black',
  #               size = 1,
  #               pch = 16) +
  #   geom_violin(alpha = 0.7, 
  #               fill = '#9B9B9B',
  #               linetype = 3,
  #               size = 0.01) +
  #   labs(x = '',
  #        y = 'Clarity Score',
  #        title = 'Version 2, violin in front, points in back',
  #        subtitle = paste0('50% transparency on points, 70% transparent on violin', 
  #                          '\n',
  #                          'Outline of violin is has a jagged line type and size minimized to replicate feathering.')) +
  #   theme_sfi(gM = FALSE) +
  #   scale_fill_manual(name = '',
  #                     values = make_colors(length(unique(data$federal)),
  #                                          categorical = TRUE, 
  #                                          bw = TRUE))
  # 
  # 
  # g3 <- 
  #   ggplot(data, 
  #          aes(x=federal,
  #              y=clarity_score)) + 
  #   geom_jitter(width = 0.3, 
  #               alpha = 0.5,
  #               color = '#464646',
  #               size = 1,
  #               pch = 16) +
  #   geom_violin(alpha = 0.3, 
  #               fill = 'black',
  #               linetype = 0,
  #               bw = 1) +
  #   labs(x = '',
  #        y = 'Clarity Score',
  #        title = 'Version 3, points in front, violin in back',
  #        subtitle = paste0('50% transparency on points, 30% transparent on violin', 
  #                          '\n',
  #                          'Increased standard deviation of violin density', '\n',
  #                          'The violin curve has not outline as attempt to show feathering.')) +
  #   theme_sfi() +
  #   scale_fill_manual(name = '',
  #                     values = make_colors(length(unique(data$federal)),
  #                                          categorical = TRUE, 
  #                                          bw = TRUE))
  # 
  # 
  # # VERSION 4
  # data$alpha <- rescale(data$clarity_score, to = c(0,1))
  # g4 <- 
  #   ggplot(data, 
  #          aes(x=federal,
  #              y=clarity_score)) + 
  #   geom_jitter(width = 0.3, 
  #               alpha = 0.5,
  #               color = 'black',
  #               size = 1,
  #               pch = 16) +
  #   geom_violin(aes(alpha = 0.5), 
  #               fill = '#9B9B9B',
  #               color = 'white',
  #               linetype = 1,
  #               size = 0.3,
  #               bw = 0.5,
  #               draw_quantiles = c(0.25, 0.5, 0.75)) +
  #   scale_alpha(name = '',
  #               range = c(0,1)) +
  #   labs(x = '',
  #        y = 'Clarity Score',
  #        title = 'Version 4, violin in front, points in back',
  #        subtitle = paste0('50% transparency on points, 70% transparent on violin', 
  #                          '\n',
  #                          'Outline of violin is has a jagged line type and size minimized to replicate feathering.')) +
  #   theme_sfi(gM = FALSE) +
  #   scale_fill_manual(name = '',
  #                     values = make_colors(length(unique(data$federal)),
  #                                          categorical = TRUE, 
  #                                          bw = TRUE))
  # 
  # 
  # 
  # g5 <- 
  #   ggplot(data, 
  #          aes(x=federal,
  #              y=clarity_score)) + 
  #   geom_jitter(width = 0.3, 
  #               aes(alpha = alpha),
  #               color = '#6C6C6C',
  #               size = 2,
  #               pch = 16) +
  #   geom_violin(alpha = 0.7, 
  #               fill = 'black',
  #               linetype = 0,
  #               size = 0.3) +
  #   scale_alpha(name = '',
  #               range = c(0.3, 1)) +
  #   guides(legend = FALSE) +
  #   labs(x = '',
  #        y = 'Clarity Score',
  #        title = 'Version 4, violin in front, points in back',
  #        subtitle = paste0('50% transparency on points, 70% transparent on violin', 
  #                          '\n',
  #                          'Outline of violin is has a jagged line type and size minimized to replicate feathering.')) +
  #   theme_sfi(lp = 0,
  #             gM = FALSE) +
  #   scale_fill_manual(name = '',
  #                     values = make_colors(length(unique(data$federal)),
  #                                          categorical = TRUE, 
  #                                          bw = TRUE))
  # 
  # 
  g6 <- ggplot(data, 
         aes(x = federal, 
             y = clarity_score)) +
    ylim(c(-3, 13)) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..+..scaled../3, 
                      yend=..y.., 
                      alpha=..scaled../2), 
                  size=3, 
                  color = 'darkgrey',
                  trim=FALSE) +
    stat_ydensity(geom="segment", 
                  adjust = 20,
                  scale = 'area',
                  aes(xend=..x..-..scaled../3, 
                      yend=..y.., 
                      alpha=..scaled../2), 
                  size=3, 
                  linetype = 1,
                  color = 'darkgrey',
                  trim=FALSE) +
    scale_alpha_continuous(range= c(0.00, .999)) +
    geom_jitter(size = 1,
                color = 'black',
                width = 0.3,
                alpha = 0.5,
                pch = 16) +
    theme_sfi(lp = 'none')
  


  
  g7 <- ggplot(data %>% filter(clarity_score <= 6), 
         aes(x = federal, 
             y = clarity_score)) +
    # geom_jitter(size = 1,
    #             width = 0.3,
    #             alpha = 0.5,
    #             pch = 16) +
    stat_ydensity(geom="violin", 
                  fill = 'black',
                  scale = 'area',
                  aes(alpha= clarity_score), 
                  size=0.001, 
                  trim=FALSE) +
    stat_ydensity(geom="violin", 
                  fill = 'black',
                  aes(alpha=clarity_score), 
                  size=0.001, 
                  trim=FALSE) +
    scale_alpha_continuous(range= c(0, 1)) 
  

  
  # 
  # g8 <- ggplot(data %>% filter(clarity_score < 8), 
  #        aes(x = federal, 
  #            y = clarity_score)) +
  #   ylim(c(-3, 13)) +
  #   stat_ydensity(geom="segment", 
  #                 adjust = 20,
  #                 scale = 'area',
  #                 aes(xend=..x..+..scaled../3, 
  #                     yend=..y.., 
  #                     alpha=..scaled../2), 
  #                 size=3, 
  #                 color = 'darkgrey',
  #                 trim=FALSE) +
  #   stat_ydensity(geom="segment", 
  #                 adjust = 20,
  #                 scale = 'area',
  #                 aes(xend=..x..-..scaled../3, 
  #                     yend=..y.., 
  #                     alpha=..scaled../2), 
  #                 size=3, 
  #                 linetype = 1,
  #                 color = 'darkgrey',
  #                 trim=FALSE) +
  #   scale_alpha_continuous(range= c(0.00, .999)) +
  #   geom_jitter(size = 1,
  #               color = 'black',
  #               width = 0.3,
  #               alpha = 0.5,
  #               pch = 16) +
  #   theme_sfi(lp = 'none')
  
  
  out <- list(g1, g2, g3, g4, g5)
  return(out)
  
}


