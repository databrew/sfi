
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

# webshot::install_phantomjs()

loadfonts()

#### This markdown is for Feldman figures.

# Feldman 1 (version 1)
# Get data
data <- all_data$feldman$f1 

# recode federal
data$federal <- ifelse(data$federal == '1', 'Federal', 'State')

#Version  
g1<- ggplot(data, 
            aes(x = federal, 
                y = clarity_score)) +
  geom_violin(fill = 'darkgrey',
              color = 'darkgrey',
              alpha = 1,
              trim = TRUE,
              scale = 'width') +
  geom_jitter(size = 1,
              color = 'black',
              width = 0.35,
              alpha = 0.4,
              pch = 16) +
  labs(x = '',
       y = '',
       title = '',
       caption = paste0('The violin distribution plot represents a mirrored density.',
                        '\n', 'Data points randomly "jittered" on the X axis.')) +
  scale_alpha_continuous(range= c(-0, .5)) +
  theme_sfi(lp = 'none',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))

g1
ggsave("image_files/Feldman_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


# # Feldman 1 (version 2)
# # Version 2
# g2<- ggplot(data, 
#             aes(x = federal, 
#                 y = clarity_score)) +
#   geom_jitter(size = 2,
#               color = 'black',
#               width = 0.3,
#               alpha = 0.5,
#               pch = 16) +
#   geom_violin(fill = 'darkgrey',
#               color = 'darkgrey',
#               alpha = 1) +
#   labs(x = '',
#        y = 'Clarity Score',
#        title = 'Figure 1. Federal and State Clarity Score Distribution',
#        caption = paste0('The violin distribution plot represents a mirrored density.',
#                         '\n', 'Data points randomly "jittered" on the X axis.')) +
#   scale_alpha_continuous(range= c(-0, .5)) +
#   theme_sfi(lp = 'none',
#             y_axis_title_style = 'bold',
#             x_axis_title_style = 'bold',
#             title_style = 'bold') +
#   theme(axis.text=element_text(size = 10, hjust = 1),
#         plot.title = element_text(size = 12, face = "bold"))
# 
# g2


# Feldman 2 (version 1)

# Get data
data <- all_data$feldman$f2 

# recode federal
data$federal <- ifelse(data$federal == '1', 'Federal', 'State')


#Version  
g3<- ggplot(data, 
            aes(x = federal, 
                y = words)) +
  geom_violin(fill = 'darkgrey',
              color = 'darkgrey',
              alpha = 1,
              trim = TRUE,
              scale = 'width') +
  geom_jitter(size = 1,
              color = 'black',
              width = 0.35,
              alpha = 0.4,
              pch = 16) +
  labs(x = '',
       y = 'Words',
       title = '',
       caption = paste0('The violin distribution plot represents a mirrored density.',
                        '\n', 'Data points randomly "jittered" on the X axis.', '\n',
                        'Note: 25 opinions over 20,000 words were removed from plot.')) +
  theme_sfi(lp = 'none',
            title_style = 'bold')  +
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"))

g3

ggsave("image_files/Feldman_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)

# Feldman 2 (version 2)

# # Get data
# data <- all_data$feldman$f2 
# 
# # recode federal
# data$federal <- ifelse(data$federal == '1', 'Federal', 'State')
# 
# 
# #Version  
# g4<- ggplot(data, 
#             aes(x = federal, 
#                 y = words)) +
#   geom_jitter(size = 1.5,
#               color = 'black',
#               width = 0.35,
#               alpha = 0.4,
#               pch = 16) +
#   geom_violin(fill = 'darkgrey',
#               color = adjustcolor('darkgrey', alpha.f = 0.3),
#               alpha = 0.5,
#               trim = TRUE,
#               scale = 'width') +
#   labs(x = '',
#        y = 'Words',
#        title = 'Figure 2. Federal and State Court Opinion Word Length',
#        caption = paste0('The violin distribution plot represents a mirrored density.',
#                         '\n', 'Data points randomly "jittered" on the X axis.', '\n',
#                         'Note: 25 opinions over 20,000 words were removed from plot.')) +
#   theme_sfi(lp = 'none',
#             y_axis_title_style = 'bold',
#             x_axis_title_style = 'bold',
#             title_style = 'bold')  +
#   theme(axis.text=element_text(size = 10, hjust = 1),
#         plot.title = element_text(size = 12, face = "bold"))
# 
# g4
# 
# 
