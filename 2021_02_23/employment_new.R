# install.packages(c("tidyverse", "tidymodels", "DataExplorer", "skimr", "ggtext", "prophet"))

library(tidyverse)
library(tidymodels)
library(DataExplorer)
library(skimr)
library(ggtext)
library(prophet)
# Get the Data

# Or read in the data manually

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

employed_gender <- employed %>%
  filter(race_gender %in% c("Men", "Women")) %>% 
  pivot_wider(names_from = race_gender, values_from = employ_n) 
  


earn_clean <- earn %>% 
  filter(sex %in% c("Men", "Women"),
         race %in% c("All Races", "All Origins")) %>% 
  mutate(month = case_when(quarter == 1 ~ "01",
                           quarter == 2 ~ "04",
                           quarter == 3 ~ "07",
                           quarter == 4 ~ "10",
                           TRUE ~ "99999999"),
         yearmo = glue::glue("{year}{month}01"),
         date = lubridate::as_date(yearmo, "%Y%M%d"),
         quarter_date = lubridate::yq(glue::glue("{year}: Q{quarter}"))) %>% 
  filter(age %in% c("16 to 19 years", "20 to 24 years", "25 to 34 years", 
                    "35 to 44 years", "45 to 54 years", "55 to 64 years"))


# What do the Median Weekly Earnings look like for Men and Women over time?
earn_clean %>% 
  ggplot(aes(x = date, y = median_weekly_earn, group = sex, color = sex)) +
    geom_line() +
    labs(title = "A Comparison of Median Weekly Earnings: <b style='color:#F8766D'>Men</b> vs. <b style='color:#00BFC4'>Women</b>",
         x = NULL,
         y = NULL,
         color = NULL) +
    scale_y_continuous(labels = scales::dollar) +
    facet_wrap(~age) +
    theme_minimal() +
    theme(plot.title.position = "plot",
          plot.title = element_markdown(),
          legend.position = "none")


# What do the forecasts of Median Weekly Earnings look like for Men and Women?  
# simple one gender, one age example
one_sex_age <- earn_clean %>% 
  filter(sex == "Men",
         age == "16 to 19 years") 

pr_mod <- prophet(df = one_sex_age %>% 
              select(ds = date, y = median_weekly_earn))


future_df = make_future_dataframe(pr_mod, periods = 4, freq = "quarter")

forecast <- predict(pr_mod, future_df)

plot(pr_mod, forecast) 

LENGTH_OF_FORECAST <- 12


prophet_model <- earn_clean %>%
  select(sex, age, ds = date, y = median_weekly_earn) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(pr_mod = map(.x = data, .f = function(x) prophet(df = x, weekly.seasonality = F, daily.seasonality = F)),
         future_data = map(.x = pr_mod, .f = function(x) make_future_dataframe(x, periods = LENGTH_OF_FORECAST, freq = "quarter")),
         forecast = map2(.x = pr_mod, .y = future_data,  .f = function(x, y) predict(x, y)),
         plots = map2(.x = pr_mod, .y = future_data, .f = function(x, y) plot(x, y)))


to_graph <- prophet_model %>% 
  select(sex, age, forecast) %>% 
  unnest(cols = c(forecast)) %>% 
  mutate(date = lubridate::as_date(ds)) %>% 
  left_join(earn_clean %>% 
              select(age, sex, date, median_weekly_earn), 
            by = c("sex", "age", "date"))

to_graph  %>% 
  ggplot(aes(x = ds, y = yhat, group = sex, color = sex)) +
    geom_line() +
    geom_point(data = to_graph %>% filter(lubridate::year(ds) < 2021), aes(x = ds, y = median_weekly_earn)) +
    geom_ribbon(data = to_graph %>% filter(lubridate::year(ds) >= 2021), aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.1) +  
    labs(title = "A Comparison of Median Weekly Earnings: <b style='color:#F8766D'>Men</b> vs. <b style='color:#00BFC4'>Women</b>",
        subtitle = "Points are values, the line is Prophet's forecast for MWE",
         x = NULL,
         y = NULL,
         color = NULL) +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_datetime(date_breaks = "2 year", date_label = "%Y") +
    facet_wrap(~age) +
    theme_minimal() +
    theme(plot.title.position = "plot",
          plot.title = element_markdown(),
          legend.position = "none")
