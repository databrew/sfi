library(raster)
library(ggplot2)
library(maps)
library(dplyr)
library(sfi)
options(scipen = 999)

make_g1 <- function(point_size = 2,
                    point_type = 16,
                    point_opacity = 0.5,
                    base_size = 12, 
                    nomargin = FALSE, 
                    fc = "black", 
                    gM = TRUE, 
                    gm = FALSE, 
                    gc = "grey", 
                    gl = "dashed", 
                    boxes = FALSE, 
                    bc = "white", 
                    pc = "transparent", 
                    lp = "right", 
                    axis = 1){


  if(is.null(point_size)){
    point_size = 2
  }
  
  if(is.null(point_type)){
    point_type <- 16
  }
  
  if(is.null(point_opacity)){
    point_opacity <- 0.5
  }
  
  if(is.null(base_size)){
    base_size <- 12
  }
  
  if(is.null(nomargin)){
    nomargin <- FALSE
  }
  
  if(is.null(fc)){
    fc <- 'black'
  }
  if(is.null(gM)){
    gM <- TRUE
  }
  if(is.null(gm)){
    gm <- FALSE
  }
  if(is.null(gc)){
    gc <- 'grey'
  }
  if(is.null(gl)){
    gl <- 'dashed'    
  }
  if(is.null(boxes)){
    boxes <- FALSE
    
  }
  if(is.null(bc)){
    bc <- 'white'
  }
  if(is.null(pc)){
    pc <- 'transparent'
  }
  
  if(is.null(lp)){
    lp <- 'right'
  }
  if(is.null(axis)){
    axis <- 1
  }


      
  df <- maps::canada.cities %>%
    dplyr::select(name, pop, lat, long)
  
  label_df <- df %>%
    arrange(desc(pop))
  df$legend <- 
    ifelse(df$name %in% label_df$name[1:8],
           df$name,
           'Other')
  
  g1 <- 
    ggplot(data = df,
           aes(x = lat,
               y = pop)) +
    geom_point(size = point_size,
               pch = point_type,
               alpha = point_opacity,
               aes(color = legend)) +
   geom_line(stat = 'smooth', method = 'auto', lty = gl) +
    scale_y_log10() +
    labs(x = 'Population',
         y = 'Population',
         title = 'Population of Canadian cities by latitude',
         subtitle = 'Most cities are concentrated at lower latitudes (ie, near the U.S. border)') +
    theme_sfi(base_size = base_size, 
              nomargin = nomargin, 
              fc = fc, 
              gM = gM, 
              gm = gm, 
              gc = gc, 
              # gl = gl, 
              boxes = boxes, 
              bc = bc, 
              pc = pc, 
              lp = lp, 
              axis = axis) +
    scale_color_manual(name = '',
                       values = sfi::make_colors(n = 9,
                                                      # categorical = TRUE,
                                                 bw = TRUE))
  return(g1)
  # return(names(argg))
  
}
