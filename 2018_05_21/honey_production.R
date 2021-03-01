library(tidyverse)
library(ggrepel)
# file structure
data <- 'data'

honey_path <- 'honeyproduction.csv'

# honey data
honey_data <- read_csv(file.path(data, honey_path)) %>%
  mutate(state_to_print = ifelse(year != 2012 | numcol < 100000, "", state),
         states_to_print_unabbr = case_when(state_to_print == "CA" ~ "California",
            state_to_print == "ND" ~ "North\nDakota",
            state_to_print == "SD" ~ "South\nDakota",
            state_to_print == "FL" ~ "Florida",
            state_to_print == "MT" ~ "Montana",
            state_to_print == "MN" ~ "Minnesota",
            TRUE ~ ""),
         colors_for_states = as.factor(case_when(state == "CA" ~ "#3B9AB2",
            state == "ND" ~ "#78B7C5",
            state == "SD" ~ "#EBCC2A",
            state == "FL" ~ "#E1AF00",
            state == "MT" ~ "#F21A00",
            state == "MN" ~ "#F8AFA8",
            TRUE ~ "#d3d3d3")))

# six winners
top_six <- c("CA", "ND", "SD", "FL", "MT", "MN")
the_rest <- honey_data$state %>% 
  unique() %>%
  .[!honey_data$state %>% unique() %in% top_six]

# named list of colors
states_color_vec <- c("#3B9AB2","#EBCC2A", "#FE5BAC",
                      "#E1AF00", "#F21A00",  "#78B7C5",
                      rep("#d3d3d3", length(the_rest) + 7)) %>% tolower()
names(states_color_vec) <- c(top_six, the_rest, "trash1", "trash2", "trash3",
                             "trash4", "trash5", "trash6", "trash7")

# numcol: Number of honey producing colonies. Honey producing colonies are the maximum number of colonies from which honey was taken during the year. It is possible to take honey from colonies which did not survive the entire year
# yieldpercol: Honey yield per colony. Unit is pounds
# totalprod: Total production (numcol x yieldpercol). Unit is pounds
# stocks: Refers to stocks held by producers. Unit is pounds
# priceperlb: Refers to average price per pound based on expanded sales. Unit is dollars.
# prodvalue: Value of production (totalprod x priceperlb). Unit is dollars.
# Other useful information: Certain states are excluded every year (ex. CT) to avoid disclosing data for individual operations. Due to rounding, total colonies multiplied by total yield may not equal production. Also, summation of states will not equal U.S. level value of production.
# A tibble: 3 x 8
# state numcol yieldpercol totalprod  stocks priceperlb prodvalue  year
# <chr>  <dbl>       <dbl>     <dbl>   <dbl>      <dbl>     <dbl> <dbl>
# 1 AL     16000          71   1136000  159000       0.72    818000  1998
# 2 AZ     55000          60   3300000 1485000       0.64   2112000  1998
# 3 AR     53000          65   3445000 1688000       0.59   2033000  1998

colony_time_series <- ggplot(honey_data, aes(x = year, y = numcol, group = state, color = state)) +
  geom_line() +
  geom_point(data = honey_data %>% filter(year == 2012 & numcol > 100000)) +
  labs(title = toupper("Honey Colonies from 1998 - 2012 by State"),
       subtitle = '<strong>Top 6 States Listed</strong><br/><br/>
        <span style="font-size: 10pt;">
        What I Learned:<br><br/>
        (1) North Dakota took over the top spot in 2004 and remains there to this day in 2020.<br/>
        (2) The one state to edge out Minnesota in   2003 was Texas.<br/>
        (3) Many of these states enjoy bees for a season or two: ND and SD in Spring and Summer followed by CA welcoming many of those same bees for Fall and Winter.</span>',
       x = "Year",
       y = "") +
  geom_text(data = honey_data, aes(label = states_to_print_unabbr, size = 12), 
            position = position_nudge(x = 0.5, y = -10000)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1998, 2012, 2)) +
  scale_color_manual(values = states_color_vec) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Fira", face = 'bold', size = 22),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(
            margin = ggplot2::margin(t = 10, b = 20)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = 'Freshman'),
        axis.text.x = element_text(family = "Freshman", size = 12),
        axis.text.y = element_text(family = "Freshman", size = 12),
        axis.title.x = element_text(family = "Freshman", size = 16, face = "bold"),
        axis.title.y = element_text(family = "Freshman", size = 16)
  )
ggsave(plot = colony_time_series, 
       filename = file.path('plots', 'colony_time_series.png'),
       device = "png",
       width = 12,
       height = 8)
