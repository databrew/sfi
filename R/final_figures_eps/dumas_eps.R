
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

#### This markdown is for Dumas figures.


# Dumas 1a 

# Get data
data <- all_data$dumas$f1

# create a variable to represent the colors 
data$run <- ifelse(grepl('1', data$label), 'Run 1',
                   ifelse(grepl('2', data$label), 'Run 2',
                          'Run 3'))

data$filter <- ifelse(grepl('Restrictive', data$label), 
                      'Restrictive', 'Permissive')

g1 <- 
  ggplot(data, 
         aes(x = `True negative rate`,
             y =`True positive rate`,
             col = filter)) +
  geom_point(size = 1,
             alpha = 0.6) +
  geom_line(size = 0.5, 
            alpha = 1) +
  xlim(c(1, 0)) +
  ylim(c(0, 1)) +
  labs(title = '',
       subtitle = '',
       caption = paste0(''),
       x = 'True negative rate',
       y = 'True positive rate') +
  scale_color_manual(name = 'Filter type',
                     values = c('black', 'darkgrey')) +
  geom_abline(intercept = 1,
              color = 'black',
              slope = 1,
              size = 1,
              alpha = 0.45) +
  facet_wrap(~run, nrow = 3) +
  theme_sfi(lp = 'bottom') +
  theme(axis.text=element_text(size = 10, hjust = 1))


g1

ggsave("image_files/Dumas_Figure_1a.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)



# Dumas 1b

# Get data
data <- all_data$dumas$f2


# create a variable to represent the colors 
data$run <- ifelse(grepl('1', data$label), 'Run 1',
                   ifelse(grepl('2', data$label), 'Run 2',
                          'Run 3'))

data$filter <- ifelse(grepl('Restrictive', data$label, ignore.case = TRUE), 
                      'Restrictive', 'Permissive')


# --------------------------------------------------
# g1
# facet wrap
g1 <- 
  ggplot(data, 
         aes(x = `True negative rate`,
             y =`True positive rate`,
             col = filter)) +
  geom_point(size = 1,
             alpha = 0.6) +
  geom_line(size = 0.5, 
            alpha = 1) +
  xlim(c(1, 0)) +
  ylim(c(0, 1)) +
  labs(title = '',
       subtitle  = '',
       caption = paste0(''),
       x = 'True negative rate',
       y = 'True positive rate') +
  scale_color_manual(name = 'Filter type',
                     values = c('black', 'grey')) +
  geom_abline(intercept = 1,
              color = 'black',
              slope = 1,
              size = 0.7,
              alpha = 0.6) +
  facet_wrap(~run, nrow = 3) +
  theme_sfi(lp = 'bottom') +
  theme(axis.text=element_text(size = 10, hjust = 1))


g1

ggsave("image_files/Dumas_Figure_1b.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


# Dumas 2

# Get data
data <- all_data$dumas$f3


# create a variable to represent the colors 
data$run <- ifelse(grepl('1', data$label), 'Run 1',
                   ifelse(grepl('2', data$label), 'Run 2',
                          'Run 3'))

data$filter <- ifelse(grepl('with embeddings', data$label), 
                      'Permissive embeddings', 'Permissive')


## --------------------------------------------------
# g1
# facet wrap
g1 <- 
  ggplot(data, 
         aes(x = `True negative rate`,
             y =`True positive rate`,
             color = filter)) +
  geom_point(size = 1,
             alpha = 0.6) +
  geom_line(size = 1,
            alpha = .8) +
  xlim(c(1, 0)) +
  ylim(c(0, 1)) +
  scale_color_manual(name = 'Filter type',
                     values = c('black', 'grey')) +
  labs(title = '',
       subtitle = '',
       caption = paste0(''),
       x = 'True negative rate',
       y = 'True positive rate') +
  geom_abline(intercept = 1,
              color = 'black',
              slope = 1,
              size = 0.7,
              alpha = 0.6) +
  facet_wrap(~run, nrow = 3) +
  theme_sfi(lp = 'bottom') +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1
ggsave("image_files/Dumas_Figure_2a.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)



# Dumas 2b
# Get data
data <- all_data$dumas$f4


# create a variable to represent the colors 
data$run <- ifelse(grepl('1', data$label), 'Run 1',
                   ifelse(grepl('2', data$label), 'Run 2',
                          'Run 3'))

data$filter <- ifelse(grepl('with embeddings', data$label), 
                      'Permissive embeddings', 'Permissive')


## --------------------------------------------------
# g1
# facet wrap
g1  <- ggplot(data, 
             aes(x = `True negative rate`,
                 y = `True positive rate`,
                 color = filter)) +
  geom_point(size = 1, 
             alpha = 0.8) +
  geom_line(size = 1,
            alpha = 0.6) +
  xlim(c(1, 0)) +
  ylim(c(0, 1)) +
  scale_color_manual(name = 'Filter type',
                     values = c('black', 'grey')) +
  labs(title = '',
       subtitle = '',
       caption = paste0(''),
       x = 'True negative rate',
       y = 'True positive rate') +
  geom_abline(intercept = 1,
              color = 'black',
              slope = 1,
              size = 1,
              alpha = 0.6) +
  facet_wrap(~run, 
             nrow = 3) +
  theme_sfi(lp = 'bottom') +
  theme(axis.text=element_text(size = 10, hjust = 1)) 

g1


ggsave("image_files/Dumas_Figure_2b.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 1000)


