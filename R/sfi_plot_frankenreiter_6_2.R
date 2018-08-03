#' Frankenreiter 6_2
#' 
#' Generate a plot for Frankenreiter figure 6_2
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @import scales

sfi_plot_frankenreiter6_2 <- function(){
  
  # get data 
  data <- all_data$frankenreiter$f6_2
  
  # recode new 
  data$new <- ifelse(data$new == '1', 'New judge', '')
  
  # create new variable combining judgetrad and new
  data$key <- paste0(data$judgetrad, ' ', data$new)
  data$key <- Hmisc::capitalize(data$key)
  data$key <- as.factor(trimws(data$key))
  
  # group by year and key to get avg
  data <- data %>%
    group_by(year, key) %>%
    summarise(value = mean(kl.dist3))
  
  # version 1 
  g1 <- ggplot(data, 
               aes(x = year, 
                   y = value,
                   group = key,
                   linetype = key)) +
    geom_line(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = '',
         title = 'Version 1') +
    theme_sfi() + scale_color_manual(name = '',
                                    values = make_colors(length(unique(data$key)),
                                                         categorical = TRUE, 
                                                         bw = TRUE))
  
  # version 1 
  g2 <- ggplot(data, 
               aes(x = year, 
                   y = value,
                   group = key,
                   color = key)) +
    geom_line(size = 1, 
              alpha = 0.6) +
    labs(x = 'Year',
         y = '',
         title = 'Version 2') +
    theme_sfi() + scale_color_manual(name = '',
                                     values = make_colors(length(unique(data$key)),
                                                          categorical = TRUE, 
                                                          bw = TRUE))
  out <- list(g1, g2)
  
  return(out)
}