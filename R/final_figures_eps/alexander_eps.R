# Packages
library(sfi)
library(webshot)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggiraph)
library(scales)
library(tidyverse)
library(knitr)
library(Hmisc)
library(RColorBrewer)
library(extrafont)
library(kableExtra)
library(grid)
library(processx)

# Get data
data <- alexander$f4

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

# change Color to 1% instead of zero
dat_claim$value[dat_claim$key == 'Color'] <- 1

##------- USING PLOTLY
# remove dark color
# get font object
inside_f <- list(
  family = c("sans serif", "Computer modern"),
  size = 12,
  color = 'white'
)

outside_f_claim <- list(
  family = c("sans serif", "Computer modern"),
  size = 12,
  color = 'black'
)

outside_f <- list(
  family = c("sans serif", "Computer modern"),
  size = 12,
  color = 'black'
)

outside_f_origin <- list(
  family = c("sans serif", "Computer modern"),
  size = 12,
  color = 'black'
)

# text font 
t <- list(
  family = c("sans serif", "Computer modern"),
  size = 14,
  color = 'black'
)
# make label vector
temp <- paste0(dat_claim$key, '\n', dat_claim$value, '%')


# plot
g1 <- plot_ly(dat_claim,
              labels = temp, 
              values = ~value,
              type ='pie',
              hole = 0.5,
              textposition = 'outside',
              textinfo = 'label',
              rotation = 90,
              outsidetextfont = outside_f_claim,
              marker = list(colors = colors_claim),
              width = 500, 
              height = 350)  %>%
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0(''),
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

g1
# devtools::install_github("ropensci/plotly", force = TRUE)

Sys.setenv("plotly_username" = "benmbrew")
Sys.setenv("plotly_api_key" = "3HeqonChf4J1DY3KGcts")
plotly_IMAGE(g1, format = "png", out_file = "image_files/Alexander_Figure_4.png")

# make label vector
temp <- paste0(dat_origin$key, '\n', dat_origin$value, '%')

# plot
g2 <- plot_ly(dat_origin,
              labels = temp, 
              values = ~round(value,1),
              type ='pie',
              hole = 0.5,
              textposition = 'outside',
              textinfo = 'label',
              rotation = 10,
              outsidetextfont = outside_f_origin,
              width = 500, 
              height = 350,
              marker = list(colors = colors_origin))  %>%
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0(''),
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

g2

Sys.setenv("plotly_username" = "benmbrew")
Sys.setenv("plotly_api_key" = "3HeqonChf4J1DY3KGcts")
plotly_IMAGE(g2, format = "png", out_file = "image_files/Alexander_Figure_4b.png")


# # Get data
data <- alexander$f7
data$value <- data$value * 100

# get colors
colors  <- make_colors(length(unique(data$key)), bw = TRUE)

# sort data
data <- data[order(data$value, decreasing = TRUE),]


# plot
g3 <- plot_ly(data,
              labels = ~key, 
              values = ~round(value),
              type ='pie',
              hole = 0.6,
              textposition = 'outside',
              textinfo = 'percent+label',
              rotation = 120,
              outsidetextfont = outside_f,
              marker = list(colors = colors),
              width = 500, 
              height = 350)  %>%
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0(''),
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

g3 

Sys.setenv("plotly_username" = "benmbrew")
Sys.setenv("plotly_api_key" = "3HeqonChf4J1DY3KGcts")
plotly_IMAGE(g3, format = "png", out_file = "image_files/Alexander_Figure_7.png")


