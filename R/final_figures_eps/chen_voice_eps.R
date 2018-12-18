
# Packages
library(sfi)
library(kmodR)
library(directlabels)
library(webshot)
library(ggplot2)
library(dplyr)
library(ggrepel)
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

# Chen voice figure 1 version 1

# get data 
data <- all_data$chenvoice$f1

# recode variables for plot
data$qtype <- Hmisc::capitalize(data$qtype)
data$qtype <- gsub('_', ' ', data$qtype)

# #  make correlation plot 
# g1 <-ggplot(data,
#             aes(zresponse1, zresponse2)) +
#   geom_point(size = 1) +
#   labs(title = 'Correlation plot (version 1)',
#        x = '1 Mean response',
#        y = '0 Mean response') +
#   theme_sfi() +
#   theme(axis.text=element_text(size = 10, hjust = 1),
#         plot.title = element_text(size =12)) +
#   facet_wrap(~qtype, nrow = 2)
# 
# g1


# Chen voice figure 1 version 2

#  make correlation plot 
g1 <-ggplot(data,
            aes(zresponse1, zresponse2)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', 
              se = TRUE, 
              linetype = 0) +
  labs(title = '',
       x = '1 Mean response',
       y = '0 Mean response',
       caption = 'Std error estimated with a linear model') +
  theme_sfi() +
  facet_wrap(~qtype, nrow = 2) +
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size =12)) 

g1

ggsave("image_files/Chen_Voice_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)

# Chen voice figure 2 version 1

data <- all_data$chenvoice$f2
data <- as.data.frame(rbind(names(data), data))
names(data) <- c('var1', 'var2', 'value')

# recode
data$var1 <- gsub('X1', 'Both', data$var1)
data$var2 <- gsub('1', 'Intercept', data$var2)

# restructure 
data$var1 <- as.factor(data$var1)
data$var2 <- as.factor(data$var2)
data$value <- as.numeric(data$value)

# # plot
# g1 <- ggplot(data, 
#              aes(reorder(var2, -value), 
#                  value, group = var1, fill = var1)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   scale_fill_manual(name='',
#                     values = c('#AEAEAE', '#181818','#6C6C6C')) +
#   labs(title = 'Audio features', 
#        subtitle = 'feature importance (version 1)',
#        x = 'Names of features', 
#        y = 'Importance score for features') +
#   theme_sfi() + 
#   theme(axis.text=element_text(size = 10, hjust = 1),
#         axis.text.x = element_text(angle = 45),
#         plot.title = element_text(size =12)) 
# g1
# 
# 
# # Chen voice figure 2 version 2
# 
# data <- all_data$chenvoice$f2
# data <- as.data.frame(rbind(names(data), data))
# names(data) <- c('var1', 'var2', 'value')
# 
# # recode
# data$var1 <- gsub('X1', 'Both', data$var1)
# data$var2 <- gsub('1', 'Intercept', data$var2)
# 
# # restructure 
# data$var1 <- as.factor(data$var1)
# data$var2 <- as.factor(data$var2)
# data$value <- as.numeric(data$value)
# 
# # plot
# g1 <- ggplot(data, 
#              aes(reorder(var2, -value), 
#                  value, group = var1, fill = var1)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   scale_fill_manual(name='',
#                     values = c('#AEAEAE', '#181818','#6C6C6C')) +
#   labs(title = 'Audio features', 
#        subtitle = 'feature importance (version 2)',
#        x = 'Names of features', 
#        y = 'Importance score for features') +
#   coord_flip()+
#   theme_sfi() + 
#   theme(axis.text=element_text(size = 10, hjust = 1),
#         axis.text.x = element_text(),
#         plot.title = element_text(size =12)) 
# g1
# 

# Chen voice figure 2 version 3


g1 <- ggplot(data, 
             aes(reorder(var2, -value), 
                 value, 
                 group = var1)) +
  geom_point(size= 2, 
             alpha = 1,
             position = position_dodge(0.5),
             aes(color = var1)) + scale_color_manual(name='',
                                                     values = c('#AEAEAE', '#181818','#6C6C6C')) +
  geom_linerange(aes(ymin =0 , 
                     ymax =value, 
                     color = var1), 
                 size = 0.5,
                 position = position_dodge(0.5)) +
  labs(title = '', 
       caption = '',
       x = 'Names of features', 
       y = 'Importance score for features') +
  coord_flip()+
  theme_sfi(gM = FALSE) + 
  theme(axis.text=element_text(size = 10, hjust = 1),
        axis.text.x = element_text(),
        plot.title = element_text(size =12)) 
g1

ggsave("image_files/Chen_Voice_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)





