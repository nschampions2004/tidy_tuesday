library(tidyverse)
library(tidymodels)
library(skimr)
library(DataExplorer)
library(janitor)
library(zipcodeR)


data_folder <- 'data'
models_folder <- 'models'
plots_folder <- 'plots'

list.files(data_folder)

brb_zip <- "broadband_zip.csv"
brb <- 'broadband.csv'

brb_zip_data <- read_csv(file.path(data_folder, brb_zip)) %>% 
  janitor::clean_names()

brb_zip_data %>% head()

# can I get a summary by state?  
# can I get a facet_wrap by 
# state of histograms of connectivity?
nested <- brb_zip_data %>% 
  group_by(st) %>% 
  nest()

summaries <- map(nested$data, 
                        .f = function(x) summary(x$broadba