library(tidyverse)

fishing_raw = read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv"
)
stocked = read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv",
guess_max = 100000)

# to_check

fishing <- fishing_raw %>%
  filter(values >= 0) %>%
  mutate(species = str_replace(str_to_title(species), "([^s])s$", "\\1"))

summarize_fishing <- function(tbl) {
  tbl %>%
    summarize(n_obs = n(),
              total_production = sum(values, na.rm = TRUE)) %>%
    arrange(desc(total_production))
}

fishing %>%
  group_by(species) %>%
  summarize_fishing()

fishing %>%
  ggplot(aes(values + 1)) +
  geom_histogram() 
# +
#   scale_x_log10()

fishing %>%
  count(lake)
fishing %>%
  count(species, sort = TRUE)
fishing %>%
  group_by(year, lake, species) %>%
  summarize(total_values = sum(values, na.rm = TRUE),
            first_grand_total = min(grand_total, na.rm = TRUE),
            n_grand_total = n_distinct(grand_total, na.rm = TRUE)) %>%
  ungroup() %>%
  count(n_grand_total)



