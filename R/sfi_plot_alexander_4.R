#' Alexander 4
#' 
#' Generate a plot for Alexander Figure 4 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import grid
#' @import gridExtra
#' @import plotrix


sfi_plot_alexander_4 <- function(){
  
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
  
  # flipped stacked bar plot
  g1 <- 
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8,
             color = 'darkgrey') +
      coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 1') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 2.5,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)
    
  # with percentage labels
  g2 <- 
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8,
             color = 'darkgrey') +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 1') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)
  
  # with percentage labels without borders
  g3 <- 
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 1') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 3,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)
  
  # with percentage labels different font size
  g4 <- 
    ggplot(data = data %>% filter(group == 'Claim type'),
           aes(reorder(x = key, value),
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    coord_flip() +
    theme_sfi() +
    labs(x = 'Claim type',
         y = 'Percent',
         title = 'Version 1') +
    geom_text(aes(label = paste0(round(value, digits = 1), '%')),
              nudge_y = 2,
              size = 3,
              alpha = 0.6) +
    theme(axis.text.x = element_text(hjust = 0.5)) +
    ylim(0, 55)
  
  
  # ---------------------------------------------
  # waffle chart
  ggplot(data, aes(x = key, y = value, fill = category)) + 
    geom_tile(color = "black", size = 0.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
    labs(title="Waffle Chart", subtitle="'Class' of vehicles")
  
  
  # # -----------------------------------------------------------
  # # plotrix
  # ggplot(aes(fill=Source,label=Site,size=Proportion,y=c(.8,1.2,.9,1.1,.8,1.2),x=1:6),
  #        data=df[df$Site=="Site 1",])+
  #   geom_jitter(shape=21,width = .00,height = .1,alpha=1)+
  #   scale_size_area(
  #     limits=c(0,1),max_size = 32
  #   )+
  #   scale_fill_manual(values=brewer.pal(6,"Set1"))+
  #   ylim(.6,1.4)+xlim(0,7)+
  #   xlab(label=df[,"Site"])+
  #   theme(
  #     axis.ticks=element_blank(),
  #     axis.title.y=element_blank(),
  #     axis.text=element_blank()
  #   )+
  #   guides(fill=FALSE,size=F)
  # 
  
  # # ----------------------------------------------------------
  # # treemap 
  # treemap_coords <- treemapify(data, area= "value", subgroup ="key", subgroup2 = 'key')
  # ggplot(treemap_coords) + labs(title="G-20 GDP and HDI")
  
    
  # -----------------------------------------------------------  
  # grid
  #g5 <-
    ggplot(data, 
           aes(key, 
               value)) + 
    geom_tile(show.legend = FALSE
              ) +
    geom_text(aes(label = paste0(value, "%")), 
              color = "black") + theme_sfi()
  
  # grid based on size
  
  # grid with boxes
  
  # Floating bubbles
  
  # sliced (size) pie charts
  
  # verticle lines circle
  
  # joe's plot
  g2 <- 
    ggplot(data = data %>% filter(group == 'National origin') %>%
             mutate(key = gsub(' ', '\n', key)),
           aes(x = key,
               y = value)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    theme_sfi() +
    labs(x = 'National origin',
         y = 'Percent') +
    geom_text(aes(label = round(value, digits = 1)),
              nudge_y = 4,
              size = 4,
              alpha = 0.7) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 0.5)) +
    ylim(0, 75)
  
  return(list(g1, g2))
  }