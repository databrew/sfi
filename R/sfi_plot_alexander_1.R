#' Alexander 1
#' 
#' Generate a plot for Alexander Figure 1 
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra

sfi_plot_alexander_1 <- function(){
  
  # Get data
  data <- all_data$alexander$f1 %>%
    dplyr::select(-activity_number)
  
  # Modify names
  names(data) <- c('Case number', 'Date',  'Docket text')
  
  out <- 
    kable(x = data,
        format = 'latex',
        booktabs = TRUE, 
        longtable = TRUE) %>%
    # kable_styling(#latex_options = "striped",
    #               # full_width = TRUE,
    #               position = 'center') %>%
    column_spec(3, width = "30em")
  
  
  return(out)
}