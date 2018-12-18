
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
library(readr)
library(kableExtra)
library(reshape2)
library(grid)

# webshot::install_phantomjs()

loadfonts()



#### This markdown is for Eidelman figures.

# Eidelman, Kornilova, Argyle 1 (version 1)


# read in stat dict from current directory
state_dict <- read_csv('state_dict.csv')

# read in data
data <- all_data$eidelman$f1

# make states upper case
data$State <- toupper(data$State)

# join by state
data <- inner_join(data, state_dict, by = 'State')
data$State <- NULL
names(data) <- c('Pass', 'Intro', 'State')

# make data long format
data <- melt(data, id.vars = 'State')



# Eidelman, Kornilova, Argyle 1 (version 2)


# candlestick flipped
g2 <- ggplot(data, 
             aes(reorder(State, -value),
                 value, 
                 color = variable)) + 
  geom_linerange(aes(ymin =0 , 
                     ymax =value, 
                     color = variable), 
                 size = 0.5) +
  coord_flip() +
  geom_point(size= 2, 
             alpha = 1) + 
  scale_color_manual(name = '', values = c('#BFBFBF', '#000000')) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = paste0(''),
       caption = '') +
  theme_sfi(lp = 'bottom',
            gM = FALSE,
            title_style = 'bold',
            lkw = TRUE, 
            lkt = 'point', 
            legend_width = 40) +
  theme(axis.title = element_text(size = 10, hjust = 1),
        axis.text = element_text(size = 10, hjust = 1))

g2

ggsave("image_files/Eidelman_Kornilova_Argyle_Figure_1.eps", width = 6, height = 8, device=cairo_ps, fallback_resolution = 1000)

# Eidelman, Kornilova, Argyle 2 (version 1)

# # no scientific notation
# options(scipen = '999')
# 
no_zero_scale <- function(x) paste0(x*100, "%")

# read in stat dict from current directory
state_dict <- read_csv('state_dict.csv')

# # get data 
data <- all_data$eidelman$f2

# make states upper case
data$State <- toupper(data$State)
# join
data <- inner_join(data, state_dict, by = 'State')
data$State <- NULL
names(data) <- c('value','State')
# barplot

# Eidelman, Kornilova, Argyle 2 (version 2)

# # get data 

no_zero_scale <- function(x) paste0(x*100, "%")

# candlestick
g2 <- ggplot(data,
             aes(reorder(x = State,
                         -value),
                 y = value)) +
  geom_linerange(aes(ymin =0 , 
                     ymax =value), 
                 size = 0.5,
                 color = 'black') +
  geom_point(size= 2,
             alpha = 0.8) +
  scale_y_continuous(labels = no_zero_scale) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = '',
       caption = '') +
  theme_sfi(lp = 'bottom',
            base_size = 10,
            gM = FALSE,
            title_style = 'bold') +
  theme(axis.title.x=element_blank(),
        axis.ticks.length = unit(0, "lines"),
        axis.ticks.x=element_blank()) +
  coord_flip() +
  theme(axis.title = element_text(size = 10, hjust = 1),
        axis.text = element_text(size = 10, hjust = 1))
g2
ggsave("image_files/Eidelman_Kornilova_Argyle_Figure_2.eps", width = 6, height = 8, device=cairo_ps, fallback_resolution = 1000)


# Eidelman, Kornilova, Argyle 3 (version 1)

# # no scientific notation
# options(scipen = '999')
# 


# # no scientific notation
# options(scipen = '999')
no_zero_scale <- function(x) paste0(x*100, "%")
# # get data 
data <- all_data$eidelman$f3
names(data) <- c('State', 'value')
data$State <- toupper(data$State)
# candlestick
g2 <- ggplot(data,
             aes(reorder(x = State, 
                         value),
                 y = value)) +
  geom_linerange(aes(ymin =0 , 
                     ymax =value), 
                 size = 0.5,
                 color = 'black') +
  geom_point(size= 2, 
             alpha = 0.9) + 
  scale_y_continuous(labels = no_zero_scale) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = '',
       caption = '') +
  theme_sfi(lp = 'bottom',
            gM = FALSE,
            title_style = 'bold') +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 6.25, hjust = 1),
        axis.text.x = element_text(size = 8, hjust = 1)) +
  coord_flip() + geom_hline(yintercept = 0)

g2
ggsave("image_files/Eidelman_Kornilova_Argyle_Figure_3.eps", width = 6, height = 8, device=cairo_ps, fallback_resolution = 1000)


# # Eidelman, Kornilova, Argyle 3 (version 1)


no_zero_scale <- function(x) paste0(x*100, "%")

# # get data 
data <- all_data$eidelman$f4
names(data) <- c('State', 'value')
data$State <-toupper(data$State)


# candlestick
g2 <- ggplot(data,
             aes(reorder(x = State, 
                         value),
                 y = value)) +
  geom_linerange(aes(ymin =0 , 
                     ymax =value), 
                 size = 0.5,
                 color = 'black') +
  geom_point(size= 2, 
             alpha = 1) + 
  scale_y_continuous(labels = no_zero_scale) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = paste0(''),
       caption = '') +
  theme_sfi(lp = 'bottom',
            gM = FALSE,
            title_style = 'bold') +
  theme(axis.title.x=element_blank(),
        axis.ticks.length = unit(0, "lines"),
        axis.ticks.x=element_blank()) +
  theme(axis.title = element_text(size = 8, hjust = 1),
        axis.text.y = element_text(size = 6.25, hjust = 1),
        axis.text.x = element_text(size = 8, hjust = 1)) +
  coord_flip() + geom_hline(yintercept = 0)

g2

ggsave("image_files/Eidelman_Kornilova_Argyle_Figure_4.eps", width = 6, height = 9, device=cairo_ps, fallback_resolution = 1000)

# Eidelman, Kornilova, Argyle 5 (version 1)

# # get data 
data <- all_data$eidelman$f5
# data$all <- data$Baseline + data$Combined
data$State <- toupper(data$State)
data <- melt(data, id.vars = 'State')
names(data) <- c('State', 'variable','value')
data$State <- as.factor(data$State)
data$value <- round((data$value*100))

# data <- data %>% group_by(State) %>% mutate(all = sum(value))

# plot
g2 <- ggplot(data,
             aes(reorder(State, -value),
                 value,
                 color = variable)) + 
  geom_linerange(aes(ymin =0 , 
                     ymax =value, 
                     color = variable), 
                 size = 0.5) +
  coord_flip() +
  geom_point(size= 2, 
             alpha = 1) + 
  scale_color_manual(name = '', values = c('#000000', '#BFBFBF')) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = '',
       caption = '') +
  theme_sfi(lp = 'bottom',
            gM = FALSE,
            title_style = 'bold',
            lkw = TRUE, 
            lkt = 'point', 
            legend_width = 40) +
  theme(axis.title = element_text(size = 10, hjust = 1),
        axis.text.y = element_text(size = 6.25, hjust = 1),
        axis.text.x = element_text(size = 8, hjust = 1))

g2
ggsave("image_files/Eidelman_Kornilova_Argyle_Figure_5.eps", width = 6, height = 10, device=cairo_ps, fallback_resolution = 1000)

# chen ash fig 3
