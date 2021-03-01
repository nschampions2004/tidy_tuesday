library(devtools)
library(patchwork)
library(tidymodels)
library(kligon)
library(tidyverse)
library(glmnet)
library(ranger)


# data and theme
penguin_data_path <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv'

pg_data <- read_csv(penguin_data_path)

pg_data %>% head()


pg_data %>% map(.f = function(x) unique(x))

pg_clean <- pg_data %>%
  na.omit() %>%
  mutate(sex_f = as.factor(ifelse(sex == "female", 1, 0)),
         species_Adelie = ifelse(species == "Adelie", 1, 0),
         species_Gentoo = ifelse(species == "Gentoo", 1, 0),
         island_Torgersen = ifelse(species == "Torgersen", 1, 0),
         island_Biscoe = ifelse(species == "Biscoe", 1, 0)) %>%
  select(-c(species, island, sex))

initial_model_formula <- "sex ~ "
for (i in names(pg_clean)) {
  for (j in names(pg_clean)) {
    initial_model_formula <- paste(initial_model_formula,
                                   glue::glue("+ {i}*{j}"))
  }
}

col_names <- c("red", "blue", "green", "purple")
names(iris) <- col_names

iris_to_predict <- iris %>%
  select(1:3) %>%
  mutate(sex = runif(n = 1),
         purple = 0)

formula_list <- list()
initial_stem <- "sex ~ "
for (i in 1:length(col_names)) {
  if (i == 1) {
    running_interactions <- glue::glue("{col_names[i]}")
    mf <- glue::glue("{initial_stem} ({running_interactions})^2")
  } else {
    running_interactions <- glue::glue("{running_interactions}+{col_names[i]}")
    mf <- glue::glue("{initial_stem} ({running_interactions})^2")
  }

  formula_list[i] <- mf
}

true_formulas <- map(.x = formula_list,
                     .f = as.formula)
lms <- map(.x = true_formulas,
           .f = function(x) lm(formula = x, data = iris_to_predict))



interaction_creator(vector_or_list = col_names,
                    interaction_depth = 2)
