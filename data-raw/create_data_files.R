library(devtools)

# initiate raw data package
devtools::use_data_raw()

# read and create data
dummy <- data.frame(a = 1:10, b = 11:20)

devtools::use_data(dummy, overwrite = TRUE)
