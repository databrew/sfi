#' Alexander 7
#' 
#' Generate a plot for Alexander Figure 7
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import plotly

sfi_plot_alexander_7 <- function(){
#   
  # # Get data
  data <- all_data$alexander$f7
  data$value <- data$value * 100
  
  # get colors
  colors  <- make_colors(length(unique(data$key)), bw = TRUE)

  # sort data
  data <- data[order(data$value, decreasing = TRUE),]

  # get font object
  inside_f <- list(
    family = "Computer modern",
    size = 12,
    color = 'white'
  )
  
  outside_f <- list(
    family = "Computer modern",
    size = 12,
    color = colors
  )
  
  
  # text font 
  t <- list(
    family = "Computer modern",
    size = 12,
    color = '#353535'
  )
  
  
  # plot
  g1 <- plot_ly(data,
                labels = ~key, 
                values = ~round(value),
                type ='pie',
                hole = 0.6,
                textposition = 'outside',
                textinfo = 'percent+label',
                rotation = 120,
                outsidetextfont = outside_f,
                marker = list(colors = colors),
                width = 550, 
                height = 300)  %>%
    config(displayModeBar = F) %>%
    
    layout(title ='' ,
           font = t,
           showlegend = F,
           annotations = list(
             showarrow = FALSE,
             text = paste0('Figure 7', '\n', "District courst judges'" , '\n', 'actions on magistrate', '\n','judges'),
             font = t), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  g1
  
  
  return(g1)
  
}
  
  