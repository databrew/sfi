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
  
  g1 <- ggplot(data, aes(diff, similarity)) +
    geom_point(size = 1, 
               alpha = 0.9,
               color = 'black') +
    geom_smooth(method = 'loess',
                linetype = 0,
                fill = 'black',
                alpha = 0.3) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Date',
         y = '',
         title = 'Figure 3 Similarity and temporal distance',
         caption = '*Std error from local regression.') 
  
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
  
 
  g2 <- ggplot(data = data %>%
                 mutate(diff = round(diff, digits = -1)),
               aes(x = diff,
                   y = similarity)) +
    geom_violin(aes(group = factor(diff)),
                alpha = 0.2,
                fill = 'black',
                color = NA) +
    geom_jitter(alpha = 0.7,
                # pch = 1,
                size = 1) +
    ylim(0, 1) +
    theme_sfi(lp = 'none',
              y_axis_title_style = 'bold',
              x_axis_title_style = 'bold',
              title_style = 'bold') +
    labs(x = 'Date',
         y = '',
         title = 'Figure 3 Similarity and temporal distance',
         subtitle = '(Grouped every 10 years)',
         caption = '*Distribution represented with mirrored normal density (violin plot).') 
  
  

  l <- list(g1,g2)
  return(l)
}
