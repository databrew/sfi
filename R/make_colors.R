#' Make colors
#'
#' Generate a color palette of length n
#' @param n How many colors
#' @param bw Boolean, whether to make black and white
#' @param categorical Boolean, whether to make a categorical palette (alternative, numeric, assumes ordinality)
#' @param b2g Whether to use a blue to green palette (as opposed to blue to white);
#' only applicable if categorical is FALSE
#' @param r2g Whether to use a red to green palette (as opposed to blue to white); only applicable if categorical is FALSE
#' @export

make_colors <- function(n = 3, 
                        bw = FALSE,
                        categorical = FALSE, 
                        b2g = FALSE, 
                        r2g = FALSE){
  
  if(categorical){
    if(bw){
      cols <- sample(grey(seq(0.05, 0.95, by = 0.05)), 9)
    } else {
      cols <- c('#ffd700' ,'#f69500', '#f29672', '#d66857', '#9c4040', '#8d65a0', '#5e498d', '#328cc1', '#076d86', '#5cd29a', '#206535', '#5c8939', '#d0d3c5',
                '#b0acac', '#3e3e3e')
    }
  } else {
    if(b2g){
      cols <- rev(c("#2d85b7","#3196c3","#31a0c5","#30aac7","#30b5c9","#2fbfcb","#2ecbce","#33d9d1","#34d6c5","#2cd4b9","#2bd8a8","#34dd98","#7df1a6","#8afb9e","#8df08d"))
      } else if(r2g){
      cols <-c('red', "darkorange", "#8df08d", "#34dd98", "darkgreen")
      } else if(bw){
        cols <- grey(seq(0.05, 0.95, length = 9))
      } else {
        cols <- c("#16394d","#3b4e64","#02507d","#2974a0","#328cc1","#59acd7","#76b7dc","#b9dff9","#d1eaf7","#dde9ef")
    }
  }
  out <- colorRampPalette(colors = cols)(n)
  return(out)
}

