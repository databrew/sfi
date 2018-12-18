

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
## Global options

# # get data 
data <- all_data$chen$f1
# get label numbers
data$text_label <- unlist(lapply(strsplit(data$circuit, '-', fixed = TRUE), function(x) x[length(x)]))

# remove 11 with FC and SC with 12, and 0 with dc
data$text_label <- gsub('^0$', 'DC', data$text_label)
data$text_label <- gsub('12', 'FC', data$text_label)

# randomly sample half of the data set for clarity
keep_index <- sample(1:nrow(data), nrow(data)/2)
data <- data[keep_index, ]

invisible(temp <- kmod(data[, c('x', 'y')], k = 14, l = 200, i_max = 300, conv_method = "delta_C",
                       conv_error = 0, allow_empty_c = TRUE))
dat_c <- as.data.frame(cbind(data, clust_dist = temp$XC_dist_sqr_assign))

outlier_index <- as.character(temp$L_index)
# create an variable that indicates outlier
dat_c$outlier <- ifelse(dat_c$clust_dist.dist_sqr > 50, 'Outlier', 'In Cluster')
dat_c$clust_dist.c <- as.factor(as.character(dat_c$clust_dist.c))
# # recode SC and FC as in cluster

dat_2 <- dat_c

dat_2$outlier <- as.factor(ifelse(dat_2$text_label == 'SC', 'In Cluster', 
                                  ifelse(dat_2$text_label == 'FC', 'In Cluster', 
                                         ifelse(dat_2$text_label == 'DC', 'In Cluster', dat_2$outlier))))

# version 11
g1 <- ggplot(dat_c,
        aes(x = x,
            y = y,
            group = clust_dist.c,
            shape = clust_dist.c,
            color = outlier)) +
  geom_point(size = 1,
             alpha = 0.8) +
  scale_color_manual(name = '',
                     values = c('black', 'grey')) +
  scale_shape_manual(name = 'Cluster group',
                     breaks = sort(c('1', '10', '11', '12', '13', '14', '2', '3', '4', '5','6', '7', '8', '9')),
                     values = c(14, 20, 8, 19, 12, 1, 2, 3, 5, 6, 7, 10, 15, 17)) +
  
  labs(x = '',
       y = '',
       title = paste0(''),
       caption = paste0('Point shape represents belonging to a cluster' , '\n', '(K nearest neighbors clustering)')) +
  theme_sfi(lp = '',
            title_style = 'bold') +     
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size =12)) +
  annotate(geom = 'text', 
           label = 'SC',
           alpha = 0.8,
           x = -28, 
           y = 42,
           family = c("Computer modern"), 
           size =6) +
  annotate(geom = 'text', 
           label = 'FC',
           alpha = 0.8,
           x = -32, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = 'DC',
           alpha = 0.8,
           x = -16, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '9',
           alpha = 0.8,
           x = -23, 
           y = 9,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '5',
           alpha = 0.8,
           x = -7, 
           y = -35.5,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '8',
           alpha = 0.8,
           x = 16, 
           y = -38,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '6',
           alpha = 0.8,
           x = 26, 
           y = 8,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '10',
           alpha = 0.8,
           x = 23, 
           y = -15,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '4',
           alpha = 0.8,
           x = 3, 
           y = -13,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '7',
           alpha = 0.8,
           x = 13, 
           y = 18,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '2',
           x = 10, 
           y = 40,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '3',
           alpha = 0.8,
           x = -6, 
           y = 30,
           family = 'CMU Bright', 
           size =6)


g1

ggsave("image_files/Chen_Ash_Figure_1_alt.eps", width = 10, height = 10, device=cairo_ps, fallback_resolution = 2000)


# version 11
g1 <- ggplot(dat_2,
             aes(x = x,
                y = y,
                group = clust_dist.c,
                shape = clust_dist.c,
                color = outlier)) +
  geom_point(size = 1,
             alpha = 0.8) +
  scale_color_manual(name = '',
                     values = c('black', 'grey')) +
  scale_shape_manual(name = 'Cluster group',
                     breaks = sort(c('1', '10', '11', '12', '13', '14', '2', '3', '4', '5','6', '7', '8', '9')),
                     values = c(14, 20, 8, 19, 12, 1, 2, 3, 5, 6, 7, 10, 15, 17)) +
  
  labs(x = '',
       y = '',
       title = paste0(''),
       caption = paste0('')) +
  theme_sfi(lp = 'none',
            title_style = 'bold') +     
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size =12)) +
    annotate(geom = 'text', 
           label = 'SC',
           alpha = 0.8,
           x = -28, 
           y = 42,
           family = c("Computer modern"), 
           size = 6) +
  annotate(geom = 'text', 
           label = 'FC',
           alpha = 0.8,
           x = -32, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = 'DC',
           alpha = 0.8,
           x = -16, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '9',
           alpha = 0.8,
           x = -23, 
           y = 9,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '5',
           alpha = 0.8,
           x = -7, 
           y = -35.5,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '8',
           alpha = 0.8,
           x = 16, 
           y = -38,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '6',
           alpha = 0.8,
           x = 26, 
           y = 8,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '10',
           alpha = 0.8,
           x = 23, 
           y = -15,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '4',
           alpha = 0.8,
           x = 3, 
           y = -13,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '7',
           alpha = 0.8,
           x = 13, 
           y = 18,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '2',
           x = 10, 
           y = 40,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '3',
           alpha = 0.8,
           x = -6, 
           y = 30,
           family = 'CMU Bright', 
           size =6)


g1
ggsave("image_files/Chen_Ash_Figure_1.eps", width = 10, height = 10, device=cairo_ps, fallback_resolution = 2000)


# Chen and Ash 1

### K-means clustering with outlier detection

# # no scientific notation
# options(scipen = '999')
# 
# # get data 
data <- all_data$chen$f1

# get label numbers
data$text_label <- unlist(lapply(strsplit(data$circuit, '-', fixed = TRUE), function(x) x[length(x)]))

# remove 11 with FC and SC with 12, and 0 with dc
data$text_label <- gsub('^0$', 'DC', data$text_label)
data$text_label <- gsub('12', 'FC', data$text_label)

#   # Compute and plot wss for k = 2 to k = 15.
# length(unique(data$text_label))
# dat <- data[, c('x', 'y')]
# k.max <- 14
# wss <- sapply(1:k.max,
#               function(k){kmeans(dat, k, nstart=50,iter.max = 15 )$tot.withinss})
# # wss
# plot(1:k.max, wss,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# 
# 
# # K-Means Cluster Analysis
# fit <- kmeans(dat, 13) # 5 cluster solution
# # get cluster means
# aggregate(dat,by=list(fit$cluster),FUN=mean)
# # append cluster assignment
# dat_clust <- data.frame(dat, fit$cluster)

temp <- kmod(data[, c('x', 'y')], k = 13, l = 200, i_max = 300, conv_method = "delta_C",
             conv_error = 0, allow_empty_c = TRUE)
dat_c <- as.data.frame(cbind(data, clust_dist = temp$XC_dist_sqr_assign))
outlier_index <- as.character(temp$L_index)

# create an variable that indicates outlier
dat_c$outlier <- ifelse(dat_c$clust_dist.dist_sqr > 50, 'Outlier', 'In Cluster')
dat_c$clust_dist.c <- as.factor(as.character(dat_c$clust_dist.c))

# recode SC and FC as in cluster
dat_c$outlier <- ifelse(dat_c$text_label == 'SC', 'In Cluster', 
                        ifelse(dat_c$text_label == 'FC', 'In Cluster', 
                               ifelse(dat_c$text_label == 'DC', 'In Cluster', dat_c$outlier)))

# find outliers based on

# version 11
g1 <- ggplot(dat_c,
             aes(x = x,
                 y = y,
                 group = clust_dist.c,
                 color = outlier)) +
  geom_point(size = 1,
             alpha = 0.8) +
  labs(x = '',
       y = '',
       title = paste0('Figure 1: Centered by Topic-Year', '\n', 'Averaged by Judge, Labeled by Court'),
       caption = paste0('Circuit, CC Judge Vector', '\n',
                        'Demeaned by Year and Big Topic')) +
  scale_color_manual(name = '',
                     values = c('black', 'grey'))+
  theme_sfi(lp = 'bottom',
            title_style = 'bold') +     
  theme(axis.text=element_text(size = 10, hjust = 1, family = 'Computer modern'),
        plot.title = element_text(size =12)) +
  
  annotate(geom = 'text', 
           label = 'SC',
           alpha = 0.8,
           x = -28, 
           y = 42,
           family = c("Computer modern"), 
           size =6) +
  annotate(geom = 'text', 
           label = 'FC',
           alpha = 0.8,
           x = -32, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = 'DC',
           alpha = 0.8,
           x = -16, 
           y = 30,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '9',
           alpha = 0.8,
           x = -23, 
           y = 9,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '5',
           alpha = 0.8,
           x = -7, 
           y = -35.5,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '8',
           alpha = 0.8,
           x = 16, 
           y = -40,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '6',
           alpha = 0.8,
           x = 26, 
           y = 8,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '10',
           alpha = 0.8,
           x = 27, 
           y = -16,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '4',
           alpha = 0.8,
           x = 3, 
           y = -13,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '7',
           alpha = 0.8,
           x = 13, 
           y = 21,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '2',
           x = 10, 
           y = 40,
           family = 'CMU Bright', 
           size =6) +
  annotate(geom = 'text', 
           label = '3',
           alpha = 0.8,
           x = -9, 
           y = 30,
           family = 'CMU Bright', 
           size =6)


g1

ggsave("image_files/Chen_Ash_Figure_1_alt2.eps", width = 10, height = 10, device=cairo_ps, fallback_resolution = 2000)


# Chen and Ash 2

# # get data 
data <- all_data$chen$f2

# rename 
names(data) <- c('x', 'y', 'circuit','decade', 'decades')

# remove s from decades so it can be numeric
data$decades <- gsub('s', '', data$decades, fixed = TRUE)
data$decades <- as.numeric(data$decades)

# create an empty label column
data$label <- ''

# create list to store loop results
data_list <- list()

unique_years <- unique(data$decades)
for(i in 1:length(unique_years)){
  this_year <- unique_years[i]
  sub_year <- data[data$decades == this_year,]
  
  if(this_year == 1890 | this_year == 1950 | this_year == 2010) {
    
    
    # randomly assign year label to one row
    random_row <- sample(1:nrow(sub_year), 1)
    sub_year$label[random_row] <- this_year
    
  } 
  data_list[[i]] <- sub_year
  print(this_year)
}

data <- do.call('rbind', data_list)

cols <- make_colors(length(unique(data$decade)), bw = TRUE)

# version 11
g1 <- ggplot(data,
             aes(x = x,
                 y = y,
                 color = decades)) +
  ylim(c(-7, 20)) +
  geom_point(size = 2) +
  #  geom_dl(data = subset(data, decades ==  1890),
  #         aes(label = decades), method = 'last.qp', hjust  = 1) +
  # geom_dl(data = subset(data, decades ==  1950),
  #         aes(label = decades), method = 'smart.grid') +
  # geom_dl(data = subset(data, decades == 2010),
  #         aes(label = decades), method = 'last.qp') +
  geom_text_repel(aes(label = label),segment.size = 0) +
  scale_color_gradient(name = 'Year', low = "#2C2C2C", high = "#ABABAB") +  
  labs(x = '',
       y = '',
       title = 'Figure 2.',
       subtitle = 'Centered by Court Topic, Averaged by Court-Year, Labeled by Decade',
       caption = 'Court Decade, SC & CC Court Decade Vector, Demeaned by Circuit and Big Topic') +
  theme_sfi(lp = 'bottom',
            lkw = TRUE, 
            lkt = 'point', 
            legend_width = 40,
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10))


g1
ggsave("image_files/Chen_Ash_Figure_2.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)


# Chen and Ash 3

# 
# # get data 
data <- all_data$chen$f3
names(data) <- gsub('-', '.', names(data), fixed = TRUE)

data <- data[data$big.issue != 'Others',]
data <- data[data$big.issue != '9',]
data$big.issue <- as.factor(data$big.issue)
JY_levels <- levels(data$big.issue)
JY_labels <- c('1 - Criminal Appeal', '2 - Civil Rights', '3 - First Amendment','4 - Due Process',
               '5 - Privacy', '6 - Labor','7 - Regulation')
JY_labels_n <- c('1', '2', '3','4','5', '6','7')
data$big.issue.n <- data$big.issue
data$big.issue  <- factor(data$big.issue, levels=JY_levels, labels=JY_labels)


data$big.issue.n  <- factor(data$big.issue.n, levels=JY_levels, labels=JY_labels_n)

# # remove 9 and others
# data <- data %>% filter(`big-issue` != '9') %>%
#   filter(`big-issue` != 'Others')
# 
# Y_labels <- c('1 - Criminal Appeal', '2 - Civil Rights', '3 - First Amendment','4 - Due Process',
#               '5 - Privacy', '6 - Labor','7 - Regulation','9 - Miscellaneous', 'Others')
# JY_labels_n <- c('1', '2', '3','4','5', '6','7','9', 'Others')
# 
# 
# version 11
g1 <- ggplot(data,
             aes(x = x,
                 y = y)) +
  geom_point(size = 2,
             color = 'black',
             alpha = 0.4) +
  scale_color_manual(name = 'Confidence elipse',
                     values = c('darkgrey','darkgrey', 'darkgrey','darkgrey','darkgrey','darkgrey',
                                'darkgrey','darkgrey','darkgrey','darkgrey','darkgrey','darkgrey','darkgrey')) +
  labs(x = '',
       y = '',
       title = 'Figure 3.',
       subtitle = 'Centered by Judge-Year, Averaged by Topic-Year, Labeled by Topic',
       caption = paste0('Big Topics and Year, SC and CC Topic Year Vector, Demeaned by Judge & Year')) +
  annotate(geom = 'text',
           x = 0,
           y = -50,
           family = 'CMU Bright',
           label = 'Regulation') +
  annotate(geom = 'text',
           family = 'CMU Bright', 
           x = 0,
           y = 50,
           label = 'Criminal Appeal') +
  annotate(geom = 'text',
           family = 'CMU Bright', 
           x = -38,
           y = -19,
           label = '1st Amendment') +
  annotate(geom = 'text',
           family = 'CMU Bright',
           x = -40,
           y = 28,
           label = 'Civil Rights') + 
  annotate(geom = 'text',
           family = 'CMU Bright',
           x = 17,
           y = 31,
           label = 'Privacy') +
  annotate(geom = 'text',
           family = 'CMU Bright',
           x = 38,
           y = 13,
           label = 'Labor') +
  annotate(geom = 'text',
           family = 'CMU Bright',
           x = 30,
           y = -2,
           label = 'Due Process') +
  theme_sfi(lp = 'none',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1))
g1 

ggsave("image_files/Chen_Ash_Figure_3.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)

# Chen and Ash 4

# get data 
data <- all_data$chen$f4

# recode party affiliation
data <- data[data$party %in% c(1,0),]
data$party <- ifelse(data$party == 0, 'R', 'D')

# 
# version 11
g1 <- ggplot(data,
             aes(x = x,
                 y = y,
                 fill = party)) +
  geom_point(size = 2, 
             alpha = 0.7,
             pch = 21,
             color = 'black') +
  scale_fill_manual(name = '',
                    values = c('black', 'white')) +
  labs(x = '',
       y = '',
       title = '',
       subtitle = '',
       caption = '') +
  theme_sfi(lp = 'bottom',
            title_style = 'bold') +
  theme(axis.text=element_text(size = 10, hjust = 1)) + guides(col = FALSE)


g1



ggsave("image_files/Chen_Ash_Figure_4.eps", width = 6, height = 6, device=cairo_ps, fallback_resolution = 2000)

# figure 5 (use figure 4 data)
data <- all_data$chen$f1

# version 11
g1 <- ggplot(data,
             aes(x = x,
                 y = y,
                 group = cohort,
                 shape = cohort)) +
  geom_point(size = 2,
             alpha = 0.7) +
  scale_shape_manual(name = '',
                     breaks = c('1910s', '1920s', '1930s', '1940s', '1950s'),
                     values = c(0, 1, 2, 15, 16)) +
  
  labs(x = '',
       y = '',
       title = paste0(''),
       caption = paste0('')) +
  theme_sfi(lp = 'right') +     
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size =12),
        legend.text = element_text(size = 10))
g1

ggsave("image_files/Chen_Ash_Figure_5.eps", width = 8, height = 6, device=cairo_ps, fallback_resolution = 2000)


# figure 6 (use figure 4 data)
data <- all_data$chen$f1

unique(data$law_sch)[grepl('Texas', unique(data$law_sch))]


law_schools <-'University of Texas School of Law|University of Michigan Law School|University of Pennsylvania Law School|Columbia Law School|Harvard Law School|Stanford Law School|Yale Law School|University of Chicago Law School|University of Virginia School of Law'

# subset data by law school
dat_law <- data[grepl(law_schools, data$law_sch),]

# version 11
g1 <- ggplot(dat_law,
             aes(x = x,
                 y = y,
                 group = law_sch,
                 shape = law_sch)) +
  geom_point(size = 2,
             alpha = 0.7) +
  scale_shape_manual(name = '',
                     values = c(0, 1, 2, 3, 5, 6, 15, 16, 17)) +
  
  labs(x = '',
       y = '',
       title = paste0(''),
       caption = paste0('')) +
  theme_sfi(lp = 'right') +     
  theme(axis.text=element_text(size = 10, hjust = 1),
        plot.title = element_text(size =12),
        legend.text = element_text(size = 10))
g1

ggsave("image_files/Chen_Ash_Figure_6.eps", width = 9, height = 6, device=cairo_ps, fallback_resolution = 2000)


