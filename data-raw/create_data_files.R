

library(devtools)
library(readr)
library(readxl)
library(tidyverse)
library(Hmisc)

# initiate raw data package
# devtools::use_data_raw()

# get list of folders
# folder_names <- list.files('Databrew Graphics/')
folder_names <- c('Feldman', 'Laqueur and Venancio', 'Frankenreiter', 
                  'Copus et al', 'Livermore, Grom, Eidelman',
                  'Livermore, Ashley, Riddell, Carlson, Rockmore', 'Dumas', 
                  'Chen and Ash', 'Eidelman, Kornilova, Argyle', 'Chen_voice')






# --------------------
# Chen voice
chenvoice_1 <- read_csv('Databrew Graphics/Chen_voice/corr_plot.csv')

chenvoice_2 <- read_csv('Databrew Graphics/Chen_voice/FigureAudio.csv')

chenvoice_3 <- read_csv('Databrew Graphics/Chen_voice/replication.csv')


# create a list to store files
chenvoice <- list()
chenvoice$f1 <- chenvoice_1
chenvoice$f2 <- chenvoice_2
chenvoice$f3 <- chenvoice_3

# save data
devtools::use_data(chenvoice, overwrite = TRUE)

# --------------------
# Livermore, Grom, Eidelman

# read in first data set
livermoregrom_1 <- read_csv('Databrew Graphics/Livermore, Grom, Eidelman/figure_data.csv')

# create list to store data
livermoregrom <- list()
livermoregrom$f1 <- livermoregrom_1

# save data
devtools::use_data(livermoregrom, overwrite = TRUE)

# --------------------
# Eidelman, Kornilova, Argyle

# read in first data set
eidelman_1 <- read_csv('Databrew Graphics/Eidelman, Kornilova, Argyle/Figure1.csv')

# read in second data set
eidelman_2 <- read_csv('Databrew Graphics/Eidelman, Kornilova, Argyle/Figure2.csv')

# read in third data set
eidelman_3 <- read_csv('Databrew Graphics/Eidelman, Kornilova, Argyle/Figure3.csv')

# read in fourth data set
eidelman_4 <- read_csv('Databrew Graphics/Eidelman, Kornilova, Argyle/Figure4.csv')

# read in fifth data set
eidelman_5 <- read_csv('Databrew Graphics/Eidelman, Kornilova, Argyle/Figure5.csv')

# create list to store data
eidelman <- list()
eidelman$f1 <- eidelman_1
eidelman$f2 <- eidelman_2
eidelman$f3 <- eidelman_3
eidelman$f4 <- eidelman_4
eidelman$f5 <- eidelman_5

# save data list
devtools::use_data(eidelman, overwrite = TRUE)

# --------------------
# Copus et al

# unzip data 
unzip('Databrew Graphics/Copus et al/CHL-figures(1).zip', 
      exdir = 'Databrew Graphics/Copus et al')


# read in first data set
copus_1 <- read_csv('Databrew Graphics/Copus et al/Chapter.Figure1.csv')

# read in second data set
copus_2 <- read_csv('Databrew Graphics/Copus et al/Chapter.Figure2.csv')

# create list to store data
copus <- list()
copus$f1 <- copus_1
copus$f2 <- copus_2

# save list
devtools::use_data(copus, overwrite = TRUE)


# --------------------
# Chen and Ash 

# unzip data 
unzip('Databrew Graphics/Chen and Ash/for-SFI.zip', 
      exdir = 'Databrew Graphics/Chen and Ash')


# read in first data set
chen_2 <- read_csv('Databrew Graphics/Chen and Ash/CB_demeaned_vectors_for_court_decade.csv')

# read in second data set
chen_4 <- read_csv('Databrew Graphics/Chen and Ash/CBY_demeaned_vectors_for_judges.csv')

# read in 3rd data set
chen_3 <- read_csv('Databrew Graphics/Chen and Ash/JY_demeaned_vectors_for_big_issue_year.csv')

# read in 4th data set
chen_1 <- read_csv('Databrew Graphics/Chen and Ash/YB_demeaned_vectors_for_judges.csv')

# creaete list to store all data
chen <- list()
chen$f1 <- chen_1
chen$f2 <- chen_2
chen$f3 <- chen_3
chen$f4 <- chen_4

# save list
devtools::use_data(chen, overwrite = TRUE)

# --------------------
# Dumas 

# figure 1
dumas_1 <- read_csv('Databrew Graphics/Dumas/figure_one.csv')

# make capital for first letter
dumas_1$label <- Hmisc::capitalize(dumas_1$label)

# recode sensitivity to true positive rate and specificty to true negative rate
names(dumas_1) <- c('label', 'True positive rate', 'True negative rate')

# figure 2
dumas_2 <- read_csv('Databrew Graphics/Dumas/figure_two.csv')

# recode sensitivity to true positive rate and specificty to true negative rate
names(dumas_2) <- c('label', 'True positive rate', 'True negative rate')

# figure 3
dumas_3 <- read_csv('Databrew Graphics/Dumas/figure_three.csv')

# recode sensitivity to true positive rate and specificty to true negative rate
names(dumas_3) <- c('label', 'True positive rate', 'True negative rate')

# figure 4
dumas_4 <- read_csv('Databrew Graphics/Dumas/figure_four.csv')

# recode sensitivity to true positive rate and specificty to true negative rate
names(dumas_4) <- c('label', 'True positive rate', 'True negative rate')


# create list to store data
dumas <- list()
dumas$f1 <- dumas_1
dumas$f2 <- dumas_2
dumas$f3 <- dumas_3
dumas$f4 <- dumas_4
# save data
devtools::use_data(dumas, overwrite = TRUE)


# --------------------
# 'Livermore, Ashley, Riddell, Carlson, Rockmore'

# read in data, 5 datasets

# figure 1 - friendliness
livermore_1 <- read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/Friendliness.csv')

# # figure 2 
# livermore_2 <- read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/topic-15-topic-evolution.csv')

# figure 4 
livermore_2 <- read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/supreme_court_vs_appellate_accuracy.csv')

# figure 3 
livermore_3 <- read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/supreme_court_vs_appellate_court_cert_granted_accuracy.csv')

# figure 5 - topic-150-evolution
livermore_4 <- read_csv('Databrew Graphics/Livermore, Ashley, Riddell, Carlson, Rockmore/appellate_court_cert_granted_vs_appellate_court_accuracy.csv')


# make data lower case
names(livermore_1) <- tolower(names(livermore_1))
names(livermore_2) <- tolower(names(livermore_2))
names(livermore_3) <- tolower(names(livermore_3))
names(livermore_4) <- tolower(names(livermore_4))


# create list to store data
livermore <- list()
livermore$f1 <- livermore_1
livermore$f2 <- livermore_2
livermore$f3 <- livermore_3
livermore$f4 <- livermore_4

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
frankenreiter_6_1 <- read_csv('Databrew Graphics/Frankenreiter/data/figure6_1_new.csv')
frankenreiter_6_2 <- read_csv('Databrew Graphics/Frankenreiter/data/figure6_2_new.csv')


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
# alexander_et_al_1 <- read_csv('Databrew Graphics/Alexander et al/Figure 1.csv')
# alexander_et_al_1 <- alexander_et_al_1[!is.na(alexander_et_al_1$case_number),]
# 
# # read in figure 6
# alexander_et_al_6 <- read_excel('Databrew Graphics/Alexander et al/Figure 6.xlsx', sheet = 1)
# 
# # read in figures 4 and 7 and seperate, also rename columns and remove NA
# alexander_et_al_4_7 <- read_excel('Databrew Graphics/Alexander et al/Figures 4 _7.xlsx', sheet = 1)
# alexander_et_al_4 <- alexander_et_al_4_7[, 1:2]
# alexander_et_al_7 <- alexander_et_al_4_7[, 4:5]
# names(alexander_et_al_4) <- c('key', 'value')
# names(alexander_et_al_7) <- c('key', 'value')
# alexander_et_al_4 <- alexander_et_al_4[!is.na(alexander_et_al_4$key),]
# alexander_et_al_7 <- alexander_et_al_7[!is.na(alexander_et_al_7$key),]
# 
# alexander_et_al <- list()
# alexander_et_al$f1 <- alexander_et_al_1
# alexander_et_al$f4 <- alexander_et_al_4
# alexander_et_al$f6 <- alexander_et_al_6
# alexander_et_al$f7 <- alexander_et_al_7


# save folder list 
# alexander <- alexander_et_al
# devtools::use_data(alexander, overwrite = TRUE)

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
all_data$chenvoice <- chenvoice
all_data$eidelman <- eidelman
all_data$dumas <- dumas
all_data$frankenreiter <- frankenreiter
all_data$laqueur <- laqueur
all_data$livermore <- livermore
# all_data$alexander <- alexander
all_data$feldman <- feldman
all_data$chen <- chen
all_data$copus <- copus
all_data$livermoregrom <- livermoregrom

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

# # Adjust for tables
plots_dict$table <-
  ifelse(plots_dict$author == 'alexander' &
           plots_dict$figure %in% c('1', '6'),
         TRUE,
         FALSE)
devtools::use_data(plots_dict, overwrite = TRUE)
