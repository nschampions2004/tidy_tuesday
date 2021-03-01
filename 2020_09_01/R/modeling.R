library(tidyverse)
library(janitor)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv') %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv') %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv') %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv') %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv') %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))

joined_df <- key_crop_yields %>%
  left_join(fertilizer, by = c("entity", "code", "year")) %>%
  left_join(tractors,  by = c("entity", "code", "year")) %>%
  left_join(land_use,  by = c("entity", "code", "year")) %>%
  left_join(arable_land,  by = c("entity", "code", "year"))

joined_df %>% View()

