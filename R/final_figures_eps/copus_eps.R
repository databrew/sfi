
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

#### This markdown is for Copus et al figures.

# Copus et al 1 (version 1)


# # no scientific notation
# options(scipen = '999')
# 
# # get data 
data <- all_data$copus$f1

# plot point, line, smooth data
g1 <- ggplot(data, 
             aes(x, y)) +
  geom_point(size = 1,
             color = 'black', 
             alpha = 0.6) +
  geom_smooth(method = 'loess',
              linetype =0,
              fill = 'black',
              alpha = 0.4) +
  geom_smooth(method = 'lm',
              color = 'black',
              se = FALSE) +
  labs(x = '',
       y = '',
       title = '') +
  theme_sfi(lp = 'none',
            x_axis_title_style = 'bold',
            y_axis_title_style = 'bold',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1
# plot here

ggsave("image_files/copus_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


# Copus 1 et al (version 2)

#  # plot point, line, smooth data
#   g2 <- ggplot(data, 
#                aes(x, y)) +
#     geom_point(size = 1.5,
#                color = 'black', 
#                alpha = 0.5) +
#     geom_smooth(method = 'loess',
#                 linetype = 0,
#                 se = TRUE,
#                 fill = 'black',
#                 alpha = 0.6) +
#     geom_smooth(method = 'lm',
#                 color = 'darkgrey',
#                 se = FALSE,
#                 alpha = 1) +
#     labs(x = '',
#          y = '',
#          title = 'Figure 1. In Sample Prediction') +
#     theme_sfi(lp = 'none',
#               x_axis_title_style = 'bold',
#               y_axis_title_style = 'bold',
#               title_style = 'bold') +
#     theme(axis.text=element_text(size = 10, hjust = 1))
# 
# g2

# Copus et al 2a (Version 1)


# # no scientific notation
# options(scipen = '999')
# 
# # get data 
data <- all_data$copus$f2

# plot point, line, smooth data
g1 <- ggplot(data, 
             aes(Reinhardt, Leavy,
                 color = Percent.Defendant.Win)) +
  scale_color_gradient(name = 'Defendant Trial Winner %', 
                       low = 'grey', high = 'black') +
  xlim(c(0, 1)) + 
  ylim(c(0,1)) +
  xlab('Predicted Reversal Probability for Reinhardt') +
  ylab('Predicted Reversal Probability for Leavy') +
  geom_point(size = 1,
             alpha = 0.8) +
  geom_abline(intercept = 0,
              slope = 1,
              color = 'black') +
  labs(title = '') +
  theme_sfi(lp = 'bottom',
            lkw = TRUE, 
            lkt = 'point', 
            legend_width = 30,
            title_style = 'bold')  +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/copus_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)

# Copus et al 2a (Version 2)

# Copus et al 2a (Version 2)

# # no scientific notation
# options(scipen = '999')
# 
# # get data 
# data <- all_data$copus$f2
# 
# # plot point, line, smooth data
# g1 <- ggplot(data, 
#              aes(Reinhardt, Leavy,
#                  color = Percent.Defendant.Win)) +
#   scale_color_gradient(name = 'Defendant Trial Winner %', 
#                        low = 'black', high = 'darkgrey') +
#   xlim(c(0, 1)) + 
#   ylim(c(0,1)) +
#   xlab('Predicted Reversal Probability for Reinhardt') +
#   ylab('Predicted Reversal Probability for Leavy') +
#   geom_point(size = 1.2,
#              alpha = 0.6) +
#   geom_abline(intercept = 0,
#               slope = 1,
#               color = 'black') +
#   labs(title = 'Figure 2. Predicting the Votes of Ninth Circuit Judges') +
#   theme_sfi(lp = 'bottom',
#             lkw = TRUE, 
#             lkt = 'point', 
#             legend_width = 30,
#             y_axis_title_style = 'bold',
#             x_axis_title_style = 'bold',
#             title_style = 'bold')  +
#   theme(axis.text=element_text(size = 10, hjust = 1))
# 
# g1


# Copus et al 2b (Version 1)

# plot point, line, smooth data
g2 <- ggplot(data, 
             aes(Pregerson, Kleinfeld,
                 color = Percent.Defendant.Win)) +
  scale_color_gradient(name = 'Defendant Trial Winner %', 
                       low = 'grey', high = 'black') +
  xlab('Predicted Reversal Probability for Pregerson') +
  ylab('Predicted Reversal Probability for Kleinfeld') +
  xlim(c(0, 1)) + 
  ylim(c(0,1)) +
  geom_point(size = 1,
             alpha = 0.8) +
  geom_abline(intercept = 0,
              slope = 1,
              color = 'black') +
  labs(title = '') +
  theme_sfi(lp = 'bottom',
            lkw = TRUE, 
            lkt = 'point', 
            legend_width = 30,
            title_style = 'bold')  +
  theme(axis.text=element_text(size = 10, hjust = 1))
g2

ggsave("image_files/copus_Figure_2b.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)

# Copus et al 2a (Version 2)

# plot point, line, smooth data
#   g2 <- ggplot(data, 
#                aes(Pregerson, Kleinfeld,
#                    fill = Percent.Defendant.Win)) +
#     scale_fill_gradient(name = 'Defendant Trial Winner %', low = 'black', high = 'white') +
#     xlim(c(0, 1)) + 
#     ylim(c(0,1)) +
#     xlab('Predicted Reversal Probability for Pregerson') +
#     ylab('Predicted Reversal Probability for Kleinfeld') +
#     geom_point(size = 2,
#                pch = 21,
#                color = '#575757',
#                alpha = 0.8) +
#     geom_abline(intercept = 0,
#                 slope = 1,
#                 color = 'black') +
#     labs(title = 'Figure 2. Predicting the Votes of Ninth Circuit Judges') +
#     theme_sfi(lp = 'bottom',
#               lkw = TRUE, 
#               lkt = 'point', 
#               legend_width = 30,
#               y_axis_title_style = 'bold',
#               x_axis_title_style = 'bold',
#               title_style = 'bold')  +
#     theme(axis.text=element_text(size = 10, hjust = 1))
#   
# g2




