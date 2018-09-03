
library(sfi)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggiraph)
library(ggforce)
library(tidyverse)
library(scales)

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

######--------------------------------------------------------
# PLOT 3
# get a color vector
colors  <- make_colors(length(unique(dat_claim$key)), bw = TRUE)

# sort data
dat_claim <- dat_claim[order(dat_claim$value, decreasing = TRUE),]

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
        hole = 0.6,
        textposition = 'outside',
        textinfo = 'percent',
        outsidetextfont = outside_f,
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 2)))  %>%
  add_trace(dat_claim,
            labels = ~key, 
            values = ~value,
            type ='pie',
            hole = 0.6,
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

####### --------------------  make a nice barplot
# relevel key
dat_claim$key <- as.factor(dat_claim$key)
dat_claim$key <- factor(dat_claim$key, levels = c('Retaliation', 'Sex', 'Race',
                                                  'Exemption', 'National origin',
                                                  'Religion', 'Color'))
colors <- c('#000000', '#383838', '#565656', '#868686', 
  '#A2A2A2', '#C2C2C2', '#EAE8E8')
ggplot(dat_claim,
       aes(key, value,
           fill = key)) +
  geom_bar(stat= 'identity',
           color = 'grey',
           alpha = 0.7) +
  ylim(c(0, 40)) +
  xlab('Claim type') +
  ylab('Percent') +
  geom_text(aes(label = paste0(value, ' %'), vjust = -0.6), alpha = 0.7) +
  scale_fill_manual(name = '',
                    values = c('#000000', '#383838', '#565656', '#868686', 
                               '#A2A2A2', '#C2C2C2', '#EAE8E8')) +
  theme_sfi(lp = 'none')
  

####### --------------------  make a nice barplot
# relevel key
dat_claim$key <- as.factor(dat_claim$key)
dat_claim$key <- factor(dat_claim$key, levels = c('Retaliation', 'Sex', 'Race',
                                                  'Exemption', 'National origin',
                                                  'Religion', 'Color'))
colors <- c('#000000', '#383838', '#565656', '#868686', 
            '#A2A2A2', '#C2C2C2', '#EAE8E8')
ggplot(dat_claim,
       aes(key, value,
           fill = key)) +
  geom_bar(stat= 'identity',
           color = 'grey',
           alpha = 0.7) +
  ylim(c(0, 40)) +
  xlab('Claim type') +
  ylab('Percent') +
  coord_flip() +
  geom_text(aes(label = paste0(value, ' %'), hjust = -0.3), alpha = 0.7) +
  scale_fill_manual(name = '',
                    values = c('#000000', '#383838', '#565656', '#868686', 
                               '#A2A2A2', '#C2C2C2', '#EAE8E8')) +
  theme_sfi(lp = 'none')


# here you should do the line circle (mimic barplot), from youthrex


# attempt a gauge chart
dat_claim %>%
  mutate_at(vars(starts_with("v")), rescale, to=pi*c(-.5,.5), from=0:1) %>%
  ggplot()
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = 0, end = 38, 
                   fill=key)) + 
  coord_fixed() 
