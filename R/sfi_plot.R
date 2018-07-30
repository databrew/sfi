#' SFI plot
#' 
#' Generate a plot for the SFI Law as Data Project, specfic to each Author and figure type. 
#' @export
#' @return a plot object.
#' @param author Character. The last name of the publishing author.
#' @param figure Integer. The figure number in the corresponding paper.
#' @param save Boolean. Whether to save as a file or simple return the plot (the default).

sfi_plot <- function(author = 'Joe', 
                     figure = 1,  
                     save = FALSE){
  
  if(save){
    message('this function is not ready')
  } else {
    return(barplot(1:5, main = 'SFI'))
  }
  
}