#' SFI plot
#' 
#' Generate a plot for the SFI Law as Data Project, specfic to each Author and figure type. 
#' @export
#' @return a list of plots
#' @param author Character. The last name of the publishing author.
#' @param figure Character. The figure number in the corresponding paper.
#' @import ggplot2
#' @import dplyr
sfi_plot <- function(author = 'laqueur', 
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
  
  # ALEXANDER
  if(author == 'alexander'){
    # ALEXANDER FIGURE 1 
    if(figure == '1'){
      g <- sfi_plot_alexander_1()
      # ALEXANDER FIGURE 4
    } else if(figure == '4'){
      g <- sfi_plot_alexander_4()
      
      # ALEXANDER FIGURE 6
    } else if(figure == '6'){
      g <- sfi_plot_alexander_6()
      
      # ALEXANDER FIGURE 7
    } else if(figure == '7'){
      g <- sfi_plot_alexander_7()
    } 
  }
  
  # FRANKENREITER
  if(author == 'frankenreiter'){
    # FRANKENREITER FIGURE 2 
    if(figure == '2'){
      g <- sfi_plot_frankenreiter_2()
      # FRANKENREITER FIGURE 3
      
    } else if(figure == '3'){
      g <- sfi_plot_frankenreiter_3()
      
      # FRANKENREITER FIGURE 4
    } else if(figure == '4'){
      g <- sfi_plot_frankenreiter_4()
      
      # FRANKENREITER FIGURE 5_1
    } else if(figure == '5'){
      g <- sfi_plot_frankenreiter_5()
      
    }  else if(figure == '6'){
      g <- sfi_plot_frankenreiter_6()
    }
    
  }
  
  # FELDMAN
  if(author == 'feldman'){
    # FELDMAN FIGURE 1 
    if(figure == '1'){
      g <- sfi_plot_feldman_1()
      # FELDMAN FIGURE 2
    } else if(figure == '2'){
      g <- sfi_plot_feldman_2()
    }
  }
  
  # LAQUEUR
  if(author == 'laqueur'){
    # LAQUEUR FIGURE 1
    if(figure == '1'){
      g <- sfi_plot_laqueur_1()
       
    } else if(figure == '2'){
      # LAQUEUR FIGURE 2
      g <- sfi_plot_laqueur_2()
    }
  } 
  # LIVERMORE
  if(author == 'livermore'){
    # LIVMORE Figure 1
    if(figure == '1'){
      g <- sfi_plot_livermore_1()
    } 
  } 
  
  # DUMAS
  if(author == 'dumas'){
    # DUMAS FIGURE 1
    if(figure == '1'){
      g <- sfi_plot_dumas_1()
      
    } else if(figure == '2'){
      # DUMAS FIGURE 2
      g <- sfi_plot_dumas_2()
    } else if(figure =='3'){
      # DUMAS FIGURE 3
      g <- sfi_plot_dumas_3()
    } else if(figure =='4'){
      # DUMAS FIGURE 4
      g <- sfi_plot_dumas_4()
    }
    
  return(g)
}