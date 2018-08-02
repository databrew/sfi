#' Generate all visualizations
#' 
#' Generates an HTML, PDF, and individual plot files.
#' 
#' @param output_dir The directory to which the files should be written. If
#' \code{NULL} (the default), the current working directory will be used.
#' @param date A date to be printed on the report. If \code{NULL} (the
#' default), the current date will be used
#' @return new files written
#' @export 
#' @importFrom rmarkdown render 

generate_all <- function(output_dir = NULL,
                         date = NULL){
  
  # If no output directory, make current wd
  if(is.null(output_dir)){
    output_dir <- getwd()
  }
  
  # If not date, use today's
  if(is.null(date)){
    date <- Sys.Date()
  }
  
  # Create a "figures" directory if it doesn't already exist
  if(!dir.exists('figures')){
    dir.create('figures')
  }
  
  # Create a "tables" directory if it doesn't already exist
  if(!dir.exists('tables')){
    dir.create('tables')
  }
  
  # Combine parameters into a list, so as to pass to Rmd
  parameters <- list(date = date)
  
  # Find location the rmd to knit
  file_to_knit <-
    system.file('rmd/all_plots.Rmd',
                package='sfi')
  
  # Knit pdf
  rmarkdown::render(file_to_knit,
                    output_dir = output_dir,
                    output_file = 'all_plots.pdf',
                    output_format = 'pdf_document',
                    params = parameters)
  
  # # Knit html
  # rmarkdown::render(file_to_knit,
  #                   output_dir = output_dir,
  #                   output_file = 'all_plots.html',
  #                   output_format = 'html_document',
  #                   params = parameters)
}