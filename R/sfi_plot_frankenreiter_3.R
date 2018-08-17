#' Frankenreiter 3
#' 
#' Generate a plot for Frankenreiter figure 3
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggridges

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
  
  # Make plot
  g1 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_violin(aes(group = factor(diff)),
                alpha = 0.9,
                fill = 'black',
                color = NA) +
    geom_jitter(alpha = 0.5,
                pch = 1,
                size = 1) +
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
    geom_violin(aes(group = factor(diff)),
                alpha = 0.9,
                fill = 'black',
                color = NA) +
    geom_jitter(alpha = 0.5,
                # pch = 1,
                size = 1) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 2',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g3 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_violin(aes(group = factor(diff)),
                alpha = 0.6,
                fill = 'black',
                color = 'black') +
    geom_jitter(alpha = 0.5,
                # pch = 1,
                size = 1) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 3',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  g4 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_violin(aes(group = factor(diff)),
                alpha = 0.9,
                fill = 'black',
                color = NA) +
    geom_jitter(alpha = 0.7,
                # pch = 1,
                size = 0.5) +
    theme_sfi() +
    labs(x = 'Difference in years',
         y = 'Similarity',
         title = 'Version 4',
         subtitle = '(Grouped every 10 years)') +
    ylim(0, 1)
  
  l <- list(g1,g2,g3,g4)
  return(l)
}
