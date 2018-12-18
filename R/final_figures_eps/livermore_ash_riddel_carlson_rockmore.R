
  
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
## Global options
options(max.print="75")
opts_chunk$set(echo=FALSE,
               cache=FALSE,
               prompt=FALSE,
               
               
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE,
               dpi = 300,
               # dev = "cairo_pdf",
               fig.width = 7.25,
               fig.height = 4,
               dev = c("png", "cairo_pdf"),
               fig.pos="!h",
               fig.path = 'figures/')
opts_knit$set(width=75)
options(xtable.comment = FALSE)


#### This markdown is for Livermore, Ashley, Riddell, Carlson, Rockmore figures.

# Livermore, Ashley, Riddell, Carlson, Rockmore 1 (version 1)


# no scientific notation
options(scipen = '999')

# get data 
data <- all_data$livermore$f1

# version 11
g1 <- ggplot(data, 
             aes(x = median_year, 
                 y = friendscr)) +
  geom_smooth(method = 'lm', 
              linetype = 0,
              se = TRUE,
              fill = '#373737',
              alpha = 0.4) +
  geom_point(size = 1.5, 
             alpha = 0.8,
             pch = 16,
             color = 'black') +
  labs(x = '',
       y = 'Friendliness score',
       title = '',
       caption = '') +
  scale_y_continuous(labels = percent, 
                     limits = c(-0.018, 0.004),
                     breaks=c(-0.018,-0.016,-0.014,-0.012,-0.01, -0.008, 
                              -0.006, -0.004, -0.002, 0, 0.002, 0.004)) +
  theme_sfi(lp = 'none',
            title_style = 'bold') +
  geom_text(data=subset(data, justice == 'alito'),
            aes(median_year, 
                friendscr, 
                label=paste0('Justice ', Hmisc::capitalize(justice))), 
            vjust = 1.5, 
            hjust = 1)  +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/Livermore_Ash_Riddel_Carlson_Rockmore_Figure_1.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)



# Livermore, Ashley, Riddell, Carlson, Rockmore 2 (version 1)

# no scientific notation
options(scipen = '999')

# get data
data <- all_data$livermore$f2

# melt data by x1
data <- melt(data, id.vars = 'x1')

# livermore 3 
g1 <- ggplot(data,
             aes(variable, value)) +
  geom_jitter(width = 0.2,
              alpha = 0.7,
              size = 1) +
  geom_violin(fill = 'grey',
              alpha = 0.7,
              color = adjustcolor('grey', alpha.f = 0.7)) +
  labs(x = '',
       y = 'Accuracy',
       title = '',
       subtitle = '',
       caption = 'Mirrored density plot') +
  theme_sfi(lp = 'none',
            title_style = 'bold') + 
  theme(axis.text.x=element_text(angle=45, hjust=1))  +
  theme(axis.text=element_text(size = 10, hjust = 1))
g1

ggsave("image_files/Livermore_Ash_Riddel_Carlson_Rockmore_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)


# Livermore, Ashley, Riddell, Carlson, Rockmore 3 (version 1)

# no scientific notation
options(scipen = '999')

# get data
data <- all_data$livermore$f3

# melt data by x1
data <- melt(data, id.vars = 'x1')

# livermore 3
g1 <- ggplot(data,
             aes(variable, value)) +
  geom_jitter(width = 0.2,
              alpha = 0.7,
              size = 1) +
  geom_violin(fill = 'grey',
              color = 'grey',
              alpha = 0.7) +
  labs(x = '',
       y = 'Accuracy',
       title = '',
       subtitle = '',
       caption = 'Mirrored density plot') +
  theme_sfi(lp = 'none',
            title_style = 'bold') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/Livermore_Ash_Riddel_Carlson_Rockmore_Figure_3.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)


# Livermore, Ashley, Riddell, Carlson, Rockmore 4 (version 1)

# no scientific notation
options(scipen = '999')

# get data
data <- all_data$livermore$f4

# melt data by x1
data <- melt(data, id.vars = 'x1')


# livermore 4
g1 <- ggplot(data,
             aes(variable, value)) +
  geom_jitter(width = 0.2,
              alpha = 0.7,
              size = 1) +
  geom_violin(fill = 'grey',
              color = 'grey',
              alpha = 0.7) +
  labs(x = '',
       y = 'Accuracy',
       title = 'Appellate court (cert. grant) vs Appellate court',
       subtitle = 'Figure 3b. Prediction of Supreme Court and Appellate Court Opinions.',
       caption = 'Mirrored density plot') +
  theme_sfi(lp = 'none',
            title_style = 'bold') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/Livermore_Ash_Riddel_Carlson_Rockmore_Figure_4.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)

# Livermore, Ashley, Riddell, Carlson, Rockmore 4 (version 2)

# no scientific notation
options(scipen = '999')

# get data
data <- all_data$livermore$f4

# melt data by x1
data <- melt(data, id.vars = 'x1')


# livermore 4
g1 <- ggplot(data,
             aes(variable, value)) +
  geom_jitter(width = 0.2,
              alpha = 0.4,
              size = 1) +
  geom_violin(fill = 'grey',
              color = 'grey') +
  labs(x = '',
       y = 'Accuracy',
       title = 'Appellate court (cert. grant) vs Appellate court',
       subtitle = 'Figure 3b. Prediction of Supreme Court and Appellate Court Opinions.',
       caption = 'Mirrored density plot') +
  theme_sfi(lp = 'none',
            title_style = 'bold') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(axis.text=element_text(size = 10, hjust = 1))

g1

ggsave("image_files/Livermore_Ash_Riddel_Carlson_Rockmore_Figure_5.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)

