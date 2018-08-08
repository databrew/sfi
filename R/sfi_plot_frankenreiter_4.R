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
  
  spiney <- data %>%
    mutate(diff = round(diff, digits = -1)) %>%
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
  
  g1 <- ggplot(data = data %>%
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
         title = 'Version 1',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g2 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_boxplot(aes(group = diff),
                 alpha = 0.9,
                 notch = TRUE,
                 outlier.size = 0,
                 outlier.color = NA) +
    geom_jitter(alpha = 0.5, size = 0.2) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 2',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  data2 <- data %>%
    mutate(diff = round(diff, digits = -1)) %>%
    mutate(diff = factor(diff))
  g3 <- ggplot(data = data2,
                aes(x = similarity,
                    group = diff,
                    fill = diff)) +
    geom_density(alpha = 0.95) +
    scale_fill_manual(name = 'Difference\n in years',
                      values = make_colors(n = length(unique(data2$diff)),
                                           bw = TRUE)) +
    theme_sfi() +
    labs(x = 'Similarity',
         y = 'Density',
         title = 'Version 3',
         subtitle = '(Grouped every 10 years)')
  
  g4 <- ggplot(data = data2,
                aes(x = similarity,
                    group = diff,
                    fill = diff,
                    y = diff)) +
    geom_density_ridges(alpha = 0.95) +
    scale_fill_manual(name = 'Difference\n in years',
                      values = make_colors(n = length(unique(data2$diff)),
                                           bw = TRUE)) +
    theme_sfi() +
    labs(x = 'Similarity',
         y = 'Difference in years',
         title = 'Version 4',
         subtitle = '(Grouped every 10 years)') +
    theme(legend.position = 'none') +
    coord_flip()
  
  g5 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_violin(aes(group = factor(diff)),
                alpha = 0.6) +
    geom_jitter(alpha = 0.5, size = 0.2) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 5',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g6 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_jitter(alpha = 0.3, size = 0.3, width = 0.7) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 6',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g7 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_jitter(alpha = 0.9, size = 0.1, width = 2.5) +
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
    geom_jitter(alpha = 0.1, size = 0.2, width = 2) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 8',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1) +
    geom_line(data = spiney %>%
                filter(diff %in% seq(0, 60, 20)),
              aes(x = diff,
                  y = similarity),
              alpha = 0.7)
  l <- list(g1,g2,g3,g4, g5, g6, g7, g8)
  return(l)
}
