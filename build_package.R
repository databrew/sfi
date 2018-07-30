library(devtools)
library(roxygen2)
library(rmarkdown)

# rebuild data files or not 
reconstruct_data <- TRUE

# document and install the package
document('.')
install('.')

# render the README
render('README.Rmd')

# build datasets 
if(reconstruct_data){
  setwd('data-raw')
  source('create_data_files.R')
  setwd('..')
}

