#' SFI plot
#' 
#' Generate a plot for the SFI Law as Data Project, specfic to each Author and figure type. 
#' @export
#' @return a list of plots
#' @param author Character. The last name of the publishing author.
#' @param figure Character. The figure number in the corresponding paper.
#' @import ggplot2
#' @import dplyr
sfi_plot <- function(author = 'laquer', 
                     figure = '1'){
  
  dummy <- 
    ggplot() +
    theme_sfi() +
    labs(title = 'Not yet created')
  
  # Bring in all data
  ad <- sfi::all_data
  
  # Ensure author is in data
  if(!author %in% names(ad)){
    stop(paste0('The author you have selected, ', author, ', does not have data available.'))
  }
  
  # Select the data just for the author in question
  data <- ad[[author]]
  
  # Ensure that the figure number exists
  figure_name <- paste0('f', figure)
  if(!figure_name %in% names(data)){
    stop(paste0('Though the selected author, ', author, ', has available data, there is no data for figure ', figure, '.'))
  }
  data <- data[[figure_name]]
  
  # LAQUER
  if(author == 'laquer'){
    # LAQUER FIGURE 1
    if(figure == 1){
      g <- 
        ggplot(data = data,
               aes(x = year,
                   y = number_of_grants)) +
        geom_line(alpha = 0.8) +
        geom_area(alpha = 0.3) +
        theme_sfi() +
        labs(x = 'Year',
             y = 'Number of grants',
             title = 'Number of Hearings Resulting in a Grant: 1978-2015')
      g <- list(g)
    } else if(figure == 2){
      # LAQUER FIGURE 2
      g <- list(dummy)
    }
  } else {
    g <- list(dummy)
  }
  return(g)
}