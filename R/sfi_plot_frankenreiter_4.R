#' Frankenreiter 4
#' 
#' Generate a plot for Frankenreiter figure 4
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges

sfi_plot_frankenreiter_4 <- function(){
  
  # Get data
  data <- all_data$frankenreiter$f4 %>%
    filter(diff >= 0) # we are making this modification to match the vis from the paper
  
  # FILTERING OUT SOME DATA TO MATCH THEIRS

  liney <- data %>%
    group_by(diff) %>%
    summarise(similarity = mean(similarity, na.rm = TRUE)) %>%
    ungroup
  
  quanty <- data %>%
    mutate(diff = round(diff, digits = -1)) %>%
    group_by(diff) %>%
    summarise(avg = mean(similarity, na.rm = TRUE),
              med = median(similarity, na.rm = TRUE),
              q75 = quantile(similarity, 0.75, na.rm = TRUE),
              q25 = quantile(similarity, 0.25, na.rm = TRUE)) %>%
    ungroup
  
  # Make plot
  g1 <- ggplot(data = data,
         aes(x = diff,
             y = similarity)) +
    geom_point(pch = 1) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 1') +
    geom_line(data = liney) +
    ylim(0, 1)
  
  g2 <- ggplot(data = data,
               aes(x = diff,
                   y = similarity)) +
    geom_point(alpha = 0.4,
               size = 0.5) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 2') +
    geom_line(data = liney) +
    ylim(0, 1)
  
  g3 <- ggplot(data = data,
               aes(x = diff,
                   y = similarity)) +
    geom_point(alpha = 0.1,
               size = 2) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 3') +
    geom_line(data = liney) +
    ylim(0, 1)
  
  g4 <- ggplot(data = quanty) +
    geom_ribbon(aes(x = diff,
                    ymin = q25,
                    ymax = q75),
                alpha = 0.6) +
    geom_line(aes(x = diff,
                  y = med)) +
    geom_point(aes(x = diff,
                   y = med)) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 4',
         subtitle = '(Interquartile range for every 10 year period)') +
    ylim(0, 1)
  
  g5 <-  ggplot(data = data,
                aes(x = diff,
                    y = similarity)) +
    geom_point(alpha = 0.1,
               size = 2) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 5') +
    geom_smooth(color = 'black') +
    ylim(0, 1)
  
  g6 <-  ggplot(data = data,
                aes(x = diff,
                    y = similarity)) +
    geom_point(alpha = 0.5,
               size = 1,
               pch = 3) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 6') +
    geom_smooth(method = 'lm', se = FALSE,
                color = 'black', lty = 2) +
    ylim(0, 1)
  
  g7 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
         aes(x = diff,
             y = similarity)) +
    geom_jitter(alpha = 0.5, size = 0.5) +
    geom_boxplot(aes(group = diff),
                 alpha = 0.6) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 7',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g8 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_jitter(alpha = 0.5, size = 0.5) +
    geom_violin(aes(group = diff),
                 alpha = 0.6) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 8',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g9 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_boxplot(aes(group = diff),
                alpha = 0.6,
                notch = TRUE,
                outlier.size = 0.5,
                outlier.shape = 3) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 9',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  data10 <- data %>%
    mutate(diff = round(diff, digits = -1)) %>%
    mutate(diff = factor(diff))
  g10 <- ggplot(data = data10,
               aes(x = similarity,
                   group = diff,
                   fill = diff)) +
    geom_density(alpha = 0.95) +
    scale_fill_manual(name = 'Difference\n in years',
                      values = make_colors(n = length(unique(data10$diff)),
                                           bw = TRUE)) +
    theme_sfi() +
    labs(x = 'Similarity',
         y = 'Density',
         title = 'Version 10',
         subtitle = '(Grouped every 10 years)') 
  
  g11 <- ggplot(data = data10,
                aes(x = similarity,
                    group = diff,
                    fill = diff,
                    y = diff)) +
    geom_density_ridges(alpha = 0.95) +
    scale_fill_manual(name = 'Difference\n in years',
                      values = make_colors(n = length(unique(data10$diff)),
                                           bw = TRUE)) +
    theme_sfi() +
    labs(x = 'Similarity',
         y = 'Difference in years',
         title = 'Version 11',
         subtitle = '(Grouped every 10 years)') +
    theme(legend.position = 'none') 
  
  g12 <- ggplot(data = data10,
                aes(x = similarity,
                    group = diff,
                    # fill = diff,
                    y = diff)) +
    geom_density_ridges(alpha = 0.95,
                        fill = NA) +
    theme_sfi() +
    labs(x = 'Similarity',
         y = 'Difference in years',
         title = 'Version 12',
         subtitle = '(Grouped every 10 years)')
  
  return(list(g1,g2,g3,g4,g5,g6,g7,g8,g9, g10, g11, g12))
}
