library(ggrepel)
library(sfi)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggiraph)
library(ggforce)
library(tidyverse)
library(googleVis)
library(scales)


#####################################################################################################

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
# get a color vector
colors  <- make_colors(length(unique(dat_claim$key)), bw = TRUE)

# sort data
dat_claim <- dat_claim[order(dat_claim$value, decreasing = TRUE),]

# # make the zero be 0.5
dat_claim$value[dat_claim$key == 'Color'] <- 0.4


##------- USING PLOTLY
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
  size = 25,
  color = '#353535'
)


# plot
plot_ly(dat_claim,
        labels = ~key, 
        values = ~round(value),
        type ='pie',
        hole = 0.4,
        textposition = 'outside',
        textinfo = 'percent+label',
        rotation = 90,
        outsidetextfont = outside_f,
        marker = list(colors = colors))  %>%
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0('Claim type', '\n', 'Version 8'),
           font = t), 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

##------- USING ggvis

# doughnut chart
temp <- gvisPieChart(dat_claim, 
             labelvar = 'key',
             numvar = 'value',
             options = list(title="",
                            titleTextStyle="{color:'black', fontName:'Computer modern',fontSize:16}",
                            pieHole = 0.5,
                            colors = "['#0D0D0D', '#323232','#595959', '#7F7F7F', '#A5A5A5', '#CCCCCC','#F2F2F2']",
                            legend="none",
                            width=800,
                            height=500))
plot(temp)

print(temp, file="temp.html")


# gauge 


##------- USING ggplot

##------- USING base R





#####################################################################################################
# 10 plots as alternatives to pie charts
#####################################################################################################

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


# candle stick plots ---------------------------------------------------
# version 1 one
ggplot(dat_claim, 
       aes(key, value)) + 
  ylim(c(0, 50)) +
  geom_segment(aes(x=reorder(key, -value), 
                   xend=key, 
                   y=0, 
                   yend=value),
               linetype = 'dashed',
               size = 0.5,
               alpha = 0.5) + 
  geom_point(size= 25, 
             alpha = 1,
             color = c('#000000', '#383838', '#565656', '#868686', 
                       '#A2A2A2', '#BFBFBF', '#C2C2C2')) + 
  geom_text(aes(label = paste0(value, '%')),
            size = 6,
            color = 'white',
            alpha = 0.7) +
  labs(x = '',
       y = 'Percent',
       title = 'Version 1') +
  theme_sfi()

# second one ------------------
ggplot(dat_claim, 
       aes(x=key, 
           y=value)) + 
  ylim(c(0, 50)) +
  geom_segment(aes(x=reorder(key, -value), 
                   xend=key, 
                   y=0, 
                   yend=value),
               linetype = 'dashed',
               size = 0.7,
               alpha = 0.5) + 
  geom_point(size= 25,
             pch = 21,
             fill = '#383838',
             stroke = 1,
             alpha = 1,
             color = 'black') + 
  geom_text(aes(label = paste0(value, '%')),
            size = 6,
            color = 'white',
            alpha = 1) +
  labs(x = '',
       y = 'Percent',
       title = 'Version 2') +
  theme_sfi()

# third one ------------------
ggplot(dat_claim, 
       aes(x=key, 
           y=value)) + 
  ylim(c(-5, 50)) +
  geom_segment(aes(x=reorder(key, -value), 
                   xend=key, 
                   y=0, 
                   yend=value),
               linetype = 'dashed',
               size = 0.7,
               alpha = 0.5) + 
  geom_point(size= 30,
             pch = 19,
             stroke = 1,
             alpha = 1,
             color = 'darkgrey') + 
  geom_text(aes(label = paste0(value, '%')),
            size = 6,
            color = '#323232',
            alpha = 1) +
  labs(x = '',
       y = 'Percent',
       title = 'Version 3') +
  theme_sfi()

# fourth one ------------------ size of points based on value
ggplot(dat_claim, 
       aes(x=key, 
           y=value)) + 
  ylim(c(-5, 50)) +
  geom_segment(aes(x=reorder(key, -value), 
                   xend=key, 
                   y=0, 
                   yend=value),
               linetype = 'dashed',
               size = 0.7,
               alpha = 0.5) + 
  geom_point(aes(size = value),
             pch = 19,
             stroke = 1,
             alpha = 1,
             color = '#505050') + 
  geom_text(aes(label = paste0(value, '%'),
                color = reorder(key, -value)),
            size = 4,
            alpha = 1) +
  scale_size_area(name = '', 
                  max_size = 30) + 
  scale_color_manual(name = '', 
                     values= c('white', 'white', 'white', 'white','white','white',' black')) +
  labs(x = '',
       y = 'Percent',
       title = 'Version 4') +
  theme_sfi(lp = 'none')


# fifth one all on one line ------------------
ggplot(dat_claim, 
       aes(x=group, 
           y=value)) + 
  ylim(c(-5, 50)) +
  geom_segment(aes(x=group, 
                   xend=group, 
                   y=0, 
                   yend=value),
               linetype = 1,
               size = 0.3,
               alpha = 1) + 
  geom_point(aes(size = value),
             pch = 21,
             fill = 'darkgrey',
             stroke = 1,
             alpha = 1,
             color = 'black') + 
  annotate(geom = 'text', 
           label = paste0('Color', '\n', '0%'),
           x = 1.1, 
           y = 0) +
  annotate(geom = 'text', 
           label = paste0('Religion', '\n', '2%'),
           x = .88, 
           y = 2) +
  annotate(geom = 'text', 
           label = paste0('National origin', '\n', '5%'),
           x = 1.1, 
           y = 7) +
  annotate(geom = 'text', 
           label = paste0('Exemption', '\n', '5%'),
           x = .88, 
           y = 7) +
  annotate(geom = 'text', 
           label = paste0('Race', '\n', '24%'),
           x = 1.115, 
           y = 24) +
  annotate(geom = 'text', 
           label = paste0('Sex', '\n', '26%'),
           x = .88, 
           y = 26) +
  annotate(geom = 'text', 
           label = paste0('Retaliation', '\n', '38%'),
           x = 1, 
           y = 46) +
  scale_size_area(name = '', 
                  max_size = 30) + 
  labs(x = '',
       y = 'Percent',
       title = 'Version 5') +
  theme_sfi(lp = 'none')



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
  size = 25,
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
  
  layout(title = '' ,
         font = list(
           family = "CMU Bright",
           color = '#353535'),
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0('Claim type', '\n', 'Version 6'),
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
  size = 25,
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
           text = paste0('Claim type', '\n', 'Version 7'),
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
                      line = list(color = '#FFFFFF', width = 3)))  %>%
  add_trace(dat_claim,
            labels = ~key, 
            values = ~value,
            type ='pie',
            hole = 0.6,
            textposition = 'inside',
            textinfo = 'label',
            insidetextfont = inside_f,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 3))) %>%
  
  config(displayModeBar = F) %>%
  
  layout(title ='' ,
         font = t,
         showlegend = F,
         annotations = list(
           showarrow = FALSE,
           text = paste0('Claim type', '\n', 'Version 8'),
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
  ggtitle('Version 9') +
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
  ggtitle('Version 10') +
  coord_flip() +
  geom_text(aes(label = paste0(value, ' %'), hjust = -0.3), alpha = 0.7) +
  scale_fill_manual(name = '',
                    values = c('#000000', '#383838', '#565656', '#868686', 
                               '#A2A2A2', '#C2C2C2', '#EAE8E8')) +
  theme_sfi(lp = 'none')


#####################################################################################################
# Create a line chart (points) using specified theme
#####################################################################################################
# no scientific notation
options(scipen = '999')

# get data 
data <- all_data$livermore$f1

# version 1 
ggplot(data, 
       aes(x = median_year, 
           y = friendscr)) +
  geom_smooth(method = 'lm', 
              linetype = 0,
              se = TRUE,
              fill = '#373737',
              alpha = 0.4) +
  geom_point(size = 4, 
             alpha = 0.8,
             pch = 16,
             color = 'black') +
  labs(x = '',
       y = 'Friendliness score',
       title = 'Version 11',
       caption = 'Standard errors estimated with a linear regression') +
  scale_y_continuous(labels = percent, 
                     limits = c(-0.018, 0.004),
                     breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                              -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
  theme_sfi() +
  geom_text(data=subset(data, justice == 'alito'),
            aes(median_year, 
                friendscr, 
                label=paste0('Justice ', Hmisc::capitalize(justice))), 
            vjust = 1.5, 
            hjust = 1) 

# version 1 
ggplot(data, 
       aes(x = median_year, 
           y = friendscr)) +
  geom_smooth(method = 'lm', 
              linetype = 1,
              size = 5,
              se = FALSE,
              color = 'grey',
              alpha = 0.4) +
  geom_point(size = 4, 
             alpha = 0.8,
             pch = 16,
             color = 'black') +
  labs(x = '',
       y = 'Friendliness score',
       title = 'Version 12',
       caption = 'Standard errors estimated with a linear regression') +
  scale_y_continuous(labels = percent, 
                     limits = c(-0.018, 0.004),
                     breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                              -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
  theme_sfi() +
  geom_text(data=subset(data, justice == 'alito'),
            aes(median_year, 
                friendscr, 
                label=paste0('Justice ', Hmisc::capitalize(justice))), 
            vjust = 1.5, 
            hjust = 1) 


# version 3
ggplot(data, 
       aes(x = median_year, 
           y = friendscr)) +
  geom_smooth(method = 'lm', 
              linetype = 0,
              se = TRUE,
              fill = '#373737',
              alpha = 0.3) +
  geom_point(size = 4, 
             aes(alpha = -friendscr),
             pch = 16,
             color = 'black') +
  scale_alpha(name = '', 
              range = c(0.4, 1)) +
  labs(x = '',
       y = 'Friendliness score',
       title = 'Version 13',
       caption = 'Standard errors estimated with a linear regression') +
  scale_y_continuous(labels = percent, 
                     limits = c(-0.018, 0.004),
                     breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                              -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
  theme_sfi(lp = 'none') +
  geom_text(data=subset(data, justice == 'alito'),
            aes(median_year, 
                friendscr, 
                label=paste0('Justice ', Hmisc::capitalize(justice))), 
            vjust = 1.5, 
            hjust = 1) 

#####################################################################################################
# Create distributions using specified theme
#####################################################################################################
# Version 14 - this is the original, that they liked, but with slightly larger points, as they suggested.

# Get data
data <- all_data$feldman$f1 

# recode federal
data$federal <- ifelse(data$federal == '1', 'Federal', 'State')


ggplot(data, 
       aes(x = federal, 
           y = clarity_score)) +
  ylim(c(-3, 11)) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..+..scaled../3.5, 
                    yend=..y.., 
                    alpha=(..scaled../3)^5), 
                size=3, 
                color = 'darkgrey',
                trim=TRUE) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..-..scaled../3.5, 
                    yend=..y.., 
                    alpha=(..scaled../3)^5), 
                size=3, 
                linetype = 1,
                color = 'darkgrey',
                
                trim=TRUE) +
  labs(x = '',
       y = 'Clarity Score',
       title = 'Version 14, spherical cloud represents distrubition') +
  scale_alpha_continuous(range= c(-0, .5)) +
  geom_jitter(size = 1,
              color = 'black',
              width = 0.3,
              alpha = 0.3,
              pch = 16) +
  theme_sfi(lp = 'none') 


# Version 15
ggplot(data, 
       aes(x = federal, 
           y = clarity_score)) +
  ylim(c(-3, 11)) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..+..scaled../5, 
                    yend=..y.., 
                    alpha=(..scaled../3)^1), 
                size=3, 
                color = 'darkgrey',
                trim=TRUE) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..-..scaled../5, 
                    yend=..y.., 
                    alpha=(..scaled../3)^1), 
                size=3, 
                linetype = 1,
                color = 'darkgrey',
                
                trim=TRUE) +
  labs(x = '',
       y = 'Clarity Score',
       title = 'Version 15, spherical cloud represents distrubition') +
  scale_alpha_continuous(range= c(-0, .5)) +
  geom_jitter(size = 1.5,
              color = 'black',
              width = 0.2,
              alpha = 0.3,
              pch = 16) +
  theme_sfi(lp = 'none') 



 # Version 16
ggplot(data, 
       aes(x = federal, 
           y = clarity_score)) +
  ylim(c(-3, 11)) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..+..scaled../4, 
                    yend=..y.., 
                    alpha=(..scaled../3)^2), 
                size=3, 
                color = '#838383',
                trim=TRUE) +
  stat_ydensity(geom="segment", 
                adjust = 20,
                scale = 'area',
                aes(xend=..x..-..scaled../4, 
                    yend=..y.., 
                    alpha=(..scaled../3)^2), 
                size=3, 
                linetype = 1,
                color = '#838383',
                
                trim=TRUE) +
  labs(x = '',
       y = 'Clarity Score',
       title = 'Version 16, spherical cloud represents distrubition') +
  scale_alpha_continuous(range= c(-0, .5)) +
  geom_jitter(size = 2,
              color = 'black',
              width = 0.25,
              alpha = 0.25,
              pch = 16) +
  theme_sfi(lp = 'none') 


#####################################################################################################
# make plot for frankenreiter 5 so that each point colored based on time, and each decade is labeled
#####################################################################################################

# Get data
data <- all_data$frankenreiter$f5_1

cols <- make_colors(length(unique(data$year)), bw = TRUE)

# plot the 1955 to 2014 data
ggplot(data,
       aes(x = coord2,
           y = coord1,
           fill = year)) +
  geom_point(size = 4,
             pch = 21,
             alpha = 0.7) +
  theme_sfi() +
  annotate(geom = 'text',
           label = '1955',
           x = -0.0755,
           y = -0.026) +
  annotate("text",  
           x = -0.0755,
           y = -0.023,
           label = sprintf('\u2191')) +
  annotate(geom = 'text',
           label = '1960',
           x = -0.043,
           y = -0.007) +
  annotate(geom = 'text',
           label = '1960',
           x = -0.043,
           y = -0.007) + 
  annotate(geom = 'text',
           label = '1970',
           x = -0.041,
           y = 0.0084) + 
  annotate(geom = 'text',
           label = '1980',
           x = 0.0129,
           y = 0.0039) + 
  labs(x = 'Coordinate 2',
       y = 'Coordinate 1',
       title = 'Version 17') +
  scale_fill_manual(name = '', 
                     values = sort(cols, decreasing = TRUE)) +
  theme(legend.position = 'none') 
 

# plot the 1955 to 2014 data
ggplot(data,
       aes(x = coord2,
           y = coord1,
           fill = year)) +
  geom_point(size = 4,
             pch = 21,
             alpha = 0.7)  +
  ggrepel::geom_text_repel(data=subset(data, year ==  1960 | year == 1970 | year == 1980 | year == 1990 | year == 2000 | year==  2010),
                           aes(coord2, coord1, label=year), vjust = -1.5, hjust = 2.2) +
  theme_sfi() +
  labs(x = 'Coordinate 2',
       y = 'Coordinate 1',
       title = 'Version 18') +
  scale_fill_manual(name = '', 
                    values = sort(cols)) +
  theme(legend.position = 'none') 

# plot the 1955 to 2014 data
ggplot(data,
       aes(x = coord2,
           y = coord1,
           fill = year)) +
  geom_point(size = 4,
             pch = 21,
             alpha = 0.7)  +
  ggrepel::geom_text_repel(data=subset(data, year ==  1960 | year == 1970 | year == 1980 | year == 1990 | year == 2000 | year==  2010),
                           aes(coord2, coord1, label=year), vjust = -1.5, hjust = 2.2) +
  theme_sfi() +
  labs(x = 'Coordinate 2',
       y = 'Coordinate 1',
       title = 'Version 17') +
  scale_fill_manual(name = '', 
                    values = sort(cols, decreasing = TRUE)) +
  theme(legend.position = 'none') 



