
library(sfi)
library(ggplot2)
library(dplyr)
library(plotly)

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


######--------------------------------------------------------
# PLOT 1
# get a color vector
colors  <- make_colors(length(unique(dat_claim$key)), bw = TRUE)
colors <- c("#595959", "#595959", "#595959", "#595959", "#595959", "#595959", "#595959")

# remove dark color
# get font object
inside_f <- list(
  family = "CMU Bright",
  size = 25,
  color = 'white'
)

outside_f <- list(
  family = "CMU Bright",
  size = 20,
  color = colors 
)

# text font 
t <- list(
  family = "CMU Bright",
  size = 30,
  color = '#353535'
)


# plot
plot_ly(dat_claim,
        labels = ~key, 
        values = ~value,
        type ='pie',
        hole = 0.3,
        textposition = 'outside',
        textinfo = 'percent',
        outsidetextfont = outside_f,
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 2)))  %>%
  add_trace(dat_claim,
            labels = ~key, 
            values = ~value,
            type ='pie',
            hole = 0.3,
            textposition = 'inside',
            textinfo = 'label',
            insidetextfont = inside_f,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>%
  
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = 'Claim type',
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



######--------------------------------------------------------
# PLOT 2
# get a color vector
colors <- c("#989898", "#989898", "#989898", "#989898", "#989898", "#989898", "#989898")

# remove dark color
# get font object
inside_f <- list(
  family = "CMU Bright",
  size = 25,
  color = 'black'
)



# text font 
t <- list(
  family = "CMU Bright",
  size = 30,
  color = '#353535'
)


# plot
plot_ly(dat_claim,
        labels = ~key, 
        values = ~value,
        type ='pie',
        hole = 0.3,
        textposition = 'inside',
        textinfo = 'percent+label',
        outsidetextfont = outside_f,
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 2)))  %>%
  
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = 'Claim type',
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


