library(tidyverse)
library(DataExplorer)
library(correlationfunnel)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

nfl_attendance <- attendance %>%
  dplyr::left_join(standings, by = c("year", "team_name", "team"))

nfl_attendance %>% names

nfl_attendance %>%
  plot_histogram()

nfl_attendance %>%
  plot_bar()

# Questions I have:
# Can I get capacity from this data




# What does "Home" and "Away" mean and how are they categorical?
# Notes in the markdown have it as 
# Home Attendence: Doing a quick sum of weekly attendance has this as 
# the value of Home Attendence thus yearly sum of attendence at home
# Away Attendence: the sum of attendence at opponent's stadiums

# How does Strength of Schedule correlate with Margin of Victory
cor(nfl_attendance$margin_of_victory, nfl_attendance$strength_of_schedule)
# first look at strength of schedule and it's not a rank order apparent
# looking back in the data dictionary, we get: 
  # Average quality of opponent as measured by SRS (Simple Rating System)
# lets look at a really hard strength of schedule
# per: https://www.cbssports.com/nfl/news/2019-nfl-strength-of-schedule-patriots-and-redskins-have-it-easiest-raiders-face-roughest-ride/
# pats and redskins had it easiest
nfl_attendance %>%
  group_by(team_name, year) %>%
  summarize(first_sos = first(strength_of_schedule)) %>%
  filter(team_name %in% c("Redskins", "Patriots"), year == 2019)

nfl_attendance %>%
  filter(year == 2019) %>%
  group_by(team_name, year) %>%
  summarize(first_sos = first(strength_of_schedule)) %>%
  ungroup() %>%
  arrange(first_sos)

nfl_attendance %>%
  #filter(year == 201) %>%
  group_by(team_name, year) %>%
  summarize(first_sos = first(strength_of_schedule),
    sum_mov = sum(margin_of_victory)) %>%
  ungroup() %>%
  qplot(x = first_sos, y = sum_mov, data = ., color = ) +
    geom_smooth()

# according to this metric, Pats and Cowboys have the minimum sos
# ok, back to correlation
# that makes no sense in my mind, maybe there's some Simpson's Paradox going on?
# Nope, I was reading it wrong.  Teams with harder sos lose more.


# How are the New York teams handled?
nfl_attendance %>%
  filter(team == "New York")

# Can I establish who "sells out"
nfl_attendance %>% 
  View()
# Wait the value of the row level data
# doesn't have aggregated metrics and then weekly attendence stats


