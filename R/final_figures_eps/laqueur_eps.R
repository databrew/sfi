
# Packages
library(sfi)
library(webshot)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggiraph)
library(scales)
library(tidyverse)
library(directlabels)
library(knitr)
library(Hmisc)
library(gridExtra)
library(RColorBrewer)
library(extrafont)
library(kableExtra)
library(grid)
library(ggrepel)

# webshot::install_phantomjs()

loadfonts()

#### This markdown is for Laqueur and Venancio figures.

# Laqueur and Venancio 1 (version 1a)

# get data 
data <- all_data$laqueur$f1

# version 1
g1 <- 
  ggplot(data, 
         aes(x = year, 
             y = number_of_grants)) +
  geom_smooth(method = 'loess',
              alpha = 0.7,
              color = 'black',
              linetype = 0) + 
  geom_point(size = 3, 
             color = 'black',
             alpha = 0.8,
             pch = 16) +
  geom_text_repel(aes(label = number_of_grants),
                  size = 4,
                  color = 'black') +
  labs(x = '',
       y = 'Number of grants',
       title = '',
       subtitle = '',
       caption = paste0('Smoothed with a local regression', '\n', 
                        'with bands representing standard errors')) +
  theme_sfi(lp = 'none',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1))


g1
ggsave("image_files/Laqueur_Venancio_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


# # Laqueur and Venancio 1 (version 1b)
# 
# # get data 
# data <- all_data$laqueur$f1
# 
# # version 1
# g2 <- 
#   ggplot(data, 
#          aes(x = year, 
#              y = number_of_grants)) +
#   geom_smooth(method = 'loess',
#               alpha = 0.7,
#               color = 'black',
#               linetype = 0) + 
#   geom_point(size = 3, 
#              color = 'black',
#              alpha = 0.8,
#              pch = 16) +
#   geom_label_repel(aes(label = number_of_grants),
#                    size = 2,
#                    color = 'black',
#                    label.r = .40,
#                    label.padding = 0.3) +
#   labs(x = '',
#        y = 'Number of grants',
#        title = 'Figure 1',
#        subtitle = 'Number of Hearings Resulting in a Grant: 1978-2015',
#        caption = paste0('Smoothed with a local regression', '\n', 
#                         'with bands representing standard errors')) +
#   theme_sfi(lp = 'none',
#             y_axis_title_style = 'bold',
#             x_axis_title_style = 'bold',
#             title_style = 'bold') +
#   theme(axis.text=element_text(size = 10, hjust = 1))
# 
# 
# g2

# Laqueur and Venancio 1 (version 2)

# # get data 
#  data <- all_data$laqueur$f1
#  
#  # version 1
#  g1 <- 
#    ggplot(data, 
#           aes(x = year, 
#               y = number_of_grants)) +
#    geom_smooth(method = 'loess',
#                alpha = 0.7,
#                color = 'black',
#                linetype = 0) + 
#    geom_point(size = 3, 
#               color = 'black',
#               alpha = 0.8,
#               pch = 16) +
#    labs(x = '',
#         y = 'Number of grants',
#         title = 'Figure 1',
#         subtitle = 'Number of Hearings Resulting in a Grant: 1978-2015',
#         caption = paste0('Smoothed with a local regression', '\n', 
#                          'with bands representing standard errors')) +
#    theme_sfi(lp = 'none',
#              y_axis_title_style = 'bold',
#              x_axis_title_style = 'bold',
#              title_style = 'bold') +
#    theme(axis.text=element_text(size = 10, hjust = 1))
#  
# 
# g1


# Laqueur and Venancio 2 (version 1)

# get data 
data <- all_data$laqueur$f2

# version 6
g1 <- 
  ggplot(data, 
         aes(x = year, 
             y = percent_of_conducted_hearings_resulting_in_a_grant)) +
  ylim(c(0, 50)) +
  geom_smooth(method = 'lm',
              alpha = 0.4,
              fill = 'black',
              linetype = 0) + 
  geom_point(size = 4, 
             color = 'black',
             alpha = 0.8,
             pch = 16) +
  geom_text(aes(label = paste0(percent_of_conducted_hearings_resulting_in_a_grant, 
                               '%')),
            size = 4,
            color = 'black',
            nudge_y = 0,
            vjust = -2) +
  labs(x = '',
       y = '% of hearings resulting in a grant',
       title = '',
       subtitle = '',
       caption = paste0('Smoothed with a local regression', '\n', 
                        'with bands representing standard errors')) +
  theme_sfi(lp = 'none',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/Laqueur_Venancio_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


# Laqueur and Venancio 2 (version 2)

# get data 
#   data <- all_data$laqueur$f2
#   
#   # version 6
#   g1 <- 
#     ggplot(data, 
#            aes(x = year, 
#                y = percent_of_conducted_hearings_resulting_in_a_grant)) +
#     ylim(c(0, 50)) +
#     geom_smooth(method = 'lm',
#                 alpha = 0.4,
#                 fill = 'black',
#                 linetype = 0) + 
#     geom_point(size = 4, 
#                color = 'black',
#                alpha = 0.8,
#                pch = 16) +
#     labs(x = '',
#          y = '% of hearings resulting in a grant',
#          title = 'Figure 2',
#          subtitle = 'Rate of Parole Grant: 2007-2014',
#          caption = paste0('Smoothed with a local regression', '\n', 
#                           'with bands representing standard errors')) +
#     theme_sfi(lp = 'none',
#               y_axis_title_style = 'bold',
#               x_axis_title_style = 'bold',
#               title_style = 'bold') +
#     theme(axis.text=element_text(size = 10, hjust = 1))
#  
# g1

