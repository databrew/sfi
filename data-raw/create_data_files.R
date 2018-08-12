library(devtools)
library(readr)
library(readxl)
library(tidyverse)

# initiate raw data package
# devtools::use_data_raw()

# get list of folders
# folder_names <- list.files('Databrew Graphics/')
folder_names <- c('Alexander et al', 'Feldman', 'Laqueur and Venancio', 'Frankenreiter', 
                  'Livermore, Ashley, Riddell, Carlson, Rockmore', 'Dumas')

# --------------------
# 'Livermore, Ashley, Riddell, Carlson, Rockmore'

# read in data
livermore_ashley_riddell_carlson_rockmore_1 <- 
  read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/Friendliness.csv')

# make data lower case
names(livermore_ashley_riddell_carlson_rockmore_1) <- 
  tolower(names(livermore_ashley_riddell_carlson_rockmore_1))

# create list to store data
livermore_ashley_riddell_carlson_rockmore <- list()
livermore_ashley_riddell_carlson_rockmore$f1 <- 
  livermore_ashley_riddell_carlson_rockmore_1
livermore <- livermore_ashley_riddell_carlson_rockmore
# save data
devtools::use_data(livermore, overwrite = TRUE)


# --------------------
# Frankenreiter

# unzip data 
unzip('Databrew Graphics/Frankenreiter/data_Frankenreiter.zip', 
              exdir = 'Databrew Graphics/Frankenreiter/')

# read in csvs
frankenreiter_2 <- read_csv('Databrew Graphics/Frankenreiter/data/figure2.csv')
frankenreiter_3 <- read_csv('Databrew Graphics/Frankenreiter/data/figure3.csv')
frankenreiter_4 <- read_csv('Databrew Graphics/Frankenreiter/data/figure4.csv')
frankenreiter_5_1 <- read_csv('Databrew Graphics/Frankenreiter/data/figure5_1.csv')
frankenreiter_5_2 <- read_csv('Databrew Graphics/Frankenreiter/data/figure5_2.csv')
frankenreiter_6_1 <- read_csv('Databrew Graphics/Frankenreiter/data/figure6_1.csv')
frankenreiter_6_2 <- read_csv('Databrew Graphics/Frankenreiter/data/figure6_2.csv')

frankenreiter_6 <- 
  bind_rows(frankenreiter_6_1 %>% mutate(key = '1973 enlargement'),
            frankenreiter_6_2 %>% mutate(key = '1995 enlargement'))
# make column names lowercases
names(frankenreiter_2) <- tolower(names(frankenreiter_2))
names(frankenreiter_3) <- tolower(names(frankenreiter_3))
names(frankenreiter_4) <- tolower(names(frankenreiter_4))
names(frankenreiter_5_1) <- tolower(names(frankenreiter_5_1))
names(frankenreiter_5_2) <- tolower(names(frankenreiter_5_2))
names(frankenreiter_6) <- tolower(names(frankenreiter_6))

# convert year to character
frankenreiter_5_1$year <- as.character(frankenreiter_5_1$year)
frankenreiter_5_2$year <- as.character(frankenreiter_5_2$year)
names(frankenreiter_6)[names(frankenreiter_6) == 'var2'] <- 'year'
frankenreiter_6$year <- as.character(frankenreiter_6$year)

frankenreiter_5 <- 
  bind_rows(frankenreiter_5_1 %>% mutate(source = 1),
            frankenreiter_5_2 %>% mutate(source = 2))

# create list to store data
frankenreiter <- list()
frankenreiter$f2 <- frankenreiter_2
frankenreiter$f3 <- frankenreiter_3
frankenreiter$f4 <- frankenreiter_4
frankenreiter$f5 <- frankenreiter_5
frankenreiter$f5_1 <- frankenreiter_5_1
frankenreiter$f5_2 <- frankenreiter_5_1
frankenreiter$f6 <- frankenreiter_6

# save folder list 
devtools::use_data(frankenreiter, overwrite = TRUE)


# --------------------
# Laqueur and Venancio
laqueur_venancio_1 <- read_csv('Databrew Graphics/Laqueur and Venancio/NumberGrants_1972-2015.csv')
laqueur_venancio_2 <- read_csv('Databrew Graphics/Laqueur and Venancio/PercentGrants_2007-14.csv')

# recode names
names(laqueur_venancio_1) <- c('year', 'number_of_grants')
names(laqueur_venancio_2) <- c('year', 'percent_of_conducted_hearings_resulting_in_a_grant')

# restructre variable types
laqueur_venancio_1$year <- as.numeric(as.character(laqueur_venancio_1$year))
laqueur_venancio_1$number_of_grants <- as.numeric(laqueur_venancio_1$number_of_grants)

laqueur_venancio_2$year <- as.numeric(as.character(laqueur_venancio_2$year))
laqueur_venancio_2$percent_of_conducted_hearings_resulting_in_a_grant <- 
  as.numeric(gsub('%', '', laqueur_venancio_2$percent_of_conducted_hearings_resulting_in_a_grant, 
                  fixed = TRUE))

# create list and store and save
laqueur_venancio <- list()
laqueur_venancio$f1 <- laqueur_venancio_1
laqueur_venancio$f2 <- laqueur_venancio_2

# save folder list 
laqueur <- laqueur_venancio
devtools::use_data(laqueur, overwrite = TRUE)

# --------------------
# Alexander et al
# read in alexander et al and save as list: 3 data sets
alexander_et_al_1 <- read_csv('Databrew Graphics/Alexander et al/Figure 1.csv')
alexander_et_al_1 <- alexander_et_al_1[!is.na(alexander_et_al_1$case_number),]

# read in figure 6
alexander_et_al_6 <- read_excel('Databrew Graphics/Alexander et al/Figure 6.xlsx', sheet = 1)

# read in figures 4 and 7 and seperate, also rename columns and remove NA
alexander_et_al_4_7 <- read_excel('Databrew Graphics/Alexander et al/Figures 4 _7.xlsx', sheet = 1)
alexander_et_al_4 <- alexander_et_al_4_7[, 1:2]
alexander_et_al_7 <- alexander_et_al_4_7[, 4:5]
names(alexander_et_al_4) <- c('key', 'value')
names(alexander_et_al_7) <- c('key', 'value')
alexander_et_al_4 <- alexander_et_al_4[!is.na(alexander_et_al_4$key),]
alexander_et_al_7 <- alexander_et_al_7[!is.na(alexander_et_al_7$key),]

alexander_et_al <- list()
alexander_et_al$f1 <- alexander_et_al_1
alexander_et_al$f4 <- alexander_et_al_4
alexander_et_al$f6 <- alexander_et_al_6
alexander_et_al$f7 <- alexander_et_al_7


# save folder list 
alexander <- alexander_et_al
devtools::use_data(alexander, overwrite = TRUE)

# --------------------
# Feldman

# read in data
feldman_1 <- read_csv('Databrew Graphics/Feldman/Fig1Data_AF.csv')
feldman_2 <- read_csv('Databrew Graphics/Feldman/Fig2Data_AF.csv')

# name columns 
names(feldman_1) <- c('federal', 'clarity_score')

# remove rows with words over 20000
feldman_2 <- feldman_2 %>% dplyr::filter(words <= 20000)

# restructure data types
feldman_1$federal <- as.character(feldman_1$federal)
feldman_2$federal <- as.character(feldman_2$federal)

# create list and store data
feldman <- list()
feldman$f1 <- feldman_1
feldman$f2 <- feldman_2

# save folder list 
devtools::use_data(feldman, overwrite = TRUE)

# -----------
# store all author lists into one main list 
all_data <- list()
all_data$frankenreiter <- frankenreiter
all_data$laqueur <- laqueur
all_data$livermore <- livermore
all_data$alexander <- alexander
all_data$feldman <- feldman

# save folder list 
devtools::use_data(all_data, overwrite = TRUE)

# Create dataframe of all plots
plots_row <- data.frame(author = NA,
                        figure = NA)
plots_df <- list()
authors <- names(all_data)
counter <- 0
for(i in 1:length(authors)){
  this_author <- authors[i]
  message(this_author)
  sub_data <- all_data[[this_author]]
  for(j in 1:length(sub_data)){
    counter <- counter +1
    this_figure <- (gsub('f', '', names(sub_data)[j]))
    message('---', this_figure)
    out <- plots_row
    out$author <- this_author
    out$figure <- this_figure
    plots_df[[counter]] <- out
  }
}
plots_dict <- bind_rows(plots_df) %>%
  arrange(author, figure)

# Adjust for frankenreiter combination
plots_dict <- plots_dict %>%
  filter(!(author == 'frankenreiter' & figure == '5_1'),
         !(author == 'frankenreiter' & figure == '5_2'))

# Adjust for tables
plots_dict$table <-
  ifelse(plots_dict$author == 'alexander' &
           plots_dict$figure %in% c('1', '6'), 
         TRUE,
         FALSE)
devtools::use_data(plots_dict, overwrite = TRUE)
