#' Alexander 4
#' 
#' Generate a plot for Alexander Figure 4 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import plotly


sfi_plot_alexander_4 <- function(){
  
  # Get data
  data <- all_data$alexander$f4
  
  # Flag the groups
  data$group <-
    ifelse(data$key %in% 
             c('Exemption',
               'Color',
               'Race',
               'National origin',
               'Religion',
               'Sex',
               'Retaliation'), 
           'Claim type',
           'National origin')
  
  # Get percentage
  data$value <- data$value * 100
  
  # subset to claim type 
  dat_claim <- data[data$group == 'Claim type',]
  
  # subset to National origin
  dat_origin <- data[data$group == 'National origin',]
  
  # get a color vector
  colors_claim  <- make_colors(length(unique(dat_claim$key)), bw = TRUE)
  colors_origin  <- make_colors(length(unique(dat_origin$key)), bw = TRUE)
  
  # sort data
  dat_claim <- dat_claim[order(dat_claim$value, decreasing = TRUE),]
  dat_origin <- dat_origin[order(dat_origin$value, decreasing = TRUE),]
  
  
  ##------- USING PLOTLY
  # remove dark color
  # get font object
  inside_f <- list(
    family = "Computer modern",
    size = 12,
    color = 'white'
  )
  
  outside_f_claim <- list(
    family = "Computer modern",
    size = 12,
    color = colors_claim 
  )
  
  outside_f_origin <- list(
    family = "Computer modern",
    size = 12,
    color = colors_origin
  )
  
  # text font 
  t <- list(
    family = "Computer modern",
    size = 10,
    color = '#353535'
  )
  
  
  # plot
  g1 <- plot_ly(dat_claim,
          labels = ~key, 
          values = ~round(value),
          type ='pie',
          hole = 0.5,
          textposition = 'outside',
          textinfo = 'percent+label',
          rotation = 90,
          outsidetextfont = outside_f_claim,
          marker = list(colors = colors_claim),
          width = 450, 
          height = 300)  %>%
    config(displayModeBar = F) %>%
    
    layout(title ='' ,
           font = t,
           showlegend = F,
           annotations = list(
             showarrow = FALSE,
             text = paste0('Figure 4a', '\n', 'Title VII Claim types', '\n', 'from complaint text'),
             font = t), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  # plot
  g2 <- plot_ly(dat_origin,
                labels = ~key, 
                values = ~round(value),
                type ='pie',
                hole = 0.5,
                textposition = 'outside',
                textinfo = 'percent+label',
                rotation = 10,
                outsidetextfont = outside_f_origin,
                marker = list(colors = colors_origin),
                width = 450, 
                height =300)  %>%
    config(displayModeBar = F) %>%
    
    layout(title ='' ,
           font = t,
           showlegend = F,
           annotations = list(
             showarrow = FALSE,
             text = paste0('Figure 4b', '\n', "Title VII Plaintiff's race", '\n', 'from complaint text'),
             font = t), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
 
  return(list(g1,g2))
}
