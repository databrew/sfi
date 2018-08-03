#' Frankenreiter 3
#' 
#' Generate a plot for Frankenreiter figure 3
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr

sfi_plot_frankenreiter_3 <- function(){
  
  # Get data
  data <- all_data$frankenreiter$f3
  
  # FILTERING OUT SOME DATA TO MATCH THEIRS
  data <- data %>%
    filter(!(similarity < 0.3 & diff < 25)) %>%
    filter(diff <= 57)
  
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
    geom_jitter(alpha = 0.5) +
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
    geom_jitter(alpha = 0.5) +
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
  
}
