
# Packages
library(sfi)
library(webshot)
library(reshape2)
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

#### This markdown is for Livermore, Grom, Eidelman figures.

# Livermore, Grom, Eidelman 1 (version 1)


# # no scientific notation
# options(scipen = '999')
# 
# # get data 
data <- all_data$livermoregrom$f1

# get those columns 
data <- data[, c('agency_ideol', 'sent_agency', 
                 'ml_mean_compress2', 'agency_ideol_E')]

data$agency_ideol <- as.numeric(data$agency_ideol)

# make long form
data <- melt(data)
# 
# g1 <- ggplot() + 
#   geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
#                  data$value[data$variable =='ml_mean_compress2'], 
#                  size = 'Rule Sentiment'),
#              alpha = 0.9) +
#   geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
#                  data$value[data$variable =='sent_agency'],
#                  size = 'Agency Average Sentiment'), 
#              color = '#6D6D6D', alpha = 0.1) +
#   xlab('Agency Polarity') + ylab('Sentiment') +
#   theme_sfi(lp = 'bottom') +
#   scale_size_manual(name = '',
#                     values = c(4,1)) +
#   theme(axis.text=element_text(size = 10, hjust = 1))
# 
# g1
# ggsave("image_files/Livermore_Grom_Eidelman_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)
# 

g2 <- ggplot() + 
  geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
                 data$value[data$variable =='sent_agency'],
                 size = 'Agency Average Sentiment'), 
             color = '#6D6D6D', alpha = 0.1) +
  geom_smooth(aes(data$value[data$variable =='agency_ideol_E'], 
                  data$value[data$variable =='sent_agency']),
                  method = 'loess', 
              se = FALSE,
              linetype = 1,
              color = 'black',
              alpha = 1) +
  labs(title = '',
       subtitle = '',
       x = 'Agency Polarity', 
       y = 'Sentiment',
       caption = 'Trend line estimated with locally weighted smoothing') +
  theme_sfi(lp = 'none') +
  scale_size_manual(name = '',
                    values = c(4,1)) +
  theme(axis.text=element_text(size = 10, hjust = 1))

g2
ggsave("image_files/Livermore_Grom_Eidelman_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)



# g3 <- ggplot() + 
#   geom_point(aes(data$value[data$variable =='agency_ideol_E'],
#                  data$value[data$variable =='ml_mean_compress2'],
#                  size = 'Rule Sentiment'),
#              alpha = 0.7, size = 1.5, pch = 16) +
#   # geom_point(aes(data$value[data$variable =='agency_ideol_E'], 
#   #                data$value[data$variable =='sent_agency'],
#   #                size = 'Agency Average Sentiment'), 
#   #            color = '#6D6D6D', alpha = 0.1) +
#   xlab('Agency Polarity') + ylab('Sentiment') +
#   theme_sfi(lp = 'none') +
#   scale_size_manual(name = '',
#                     values = c(4,1)) +
#   theme(axis.text=element_text(size = 10, hjust = 1))
# 
# g3
# ggsave("image_files/Livermore_Grom_Eidelman_Figure_3.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)



