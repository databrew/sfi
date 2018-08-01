library(devtools)
library(roxygen2)
library(rmarkdown)

# rebuild data files or not 
reconstruct_data <- TRUE

# # Install font files
# library(extrafont)
# Install fonts first
# font_import(paths = "~/.local/share/fonts/",prompt = F)
# font_import(pattern="[CMU/cmu]")

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

