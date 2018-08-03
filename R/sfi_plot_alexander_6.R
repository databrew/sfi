#' Alexander 6
#' 
#' Generate a plot for Alexander Figure 6
#' @export
#' @return a list of plots
#' @import ggplot2
#' @import dplyr
#' @import knitr
#' @import kableExtra

sfi_plot_alexander_6 <- function(){
  
  # Get data
  data <- all_data$alexander$f6 %>%
    mutate(case_ending = ifelse(case_ending, 'Yes', 'No'))
  
  # Modify names
  names(data) <- c('Case number',  'Docket text', 'Paired order text',
                   'Filled by', 'Adoption', 'Case ended')
  
  out <- 
    kable(x = data,
          format = 'latex',
          booktabs = TRUE, 
          longtable = TRUE) %>%
    kable_styling(#latex_options = "striped",
                  full_width = TRUE,
                  position = 'center') #%>%
    # column_spec(3, width = "30em")
  
  return(list(out))
  
}
