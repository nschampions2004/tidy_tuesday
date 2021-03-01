# libraries

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

# # A tibble: 6 x 8
# species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g sex     year
# <chr>   <chr>              <dbl>         <dbl>             <dbl>       <dbl> <chr>  <dbl>
# 1 Adelie  Torgersen           39.1          18.7               181        3750 male    2007
# 2 Adelie  Torgersen           39.5          17.4               186        3800 female  2007
# 3 Adelie  Torgersen           40.3          18                 195        3250 female  2007
# 4 Adelie  Torgersen           NA            NA                  NA          NA NA      2007
# 5 Adelie  Torgersen           36.7          19.3               193        3450 female  2007
# 6 Adelie  Torgersen           39.3          20.6               190        3650 male    2007

theme_set(theme_minimal())

# How many years did studies run for?
pg_data %>% pluck("year") %>% unique()

# How many species were present throughout the studies?
pg_data %>% pluck("species") %>% unique()

# Were the entry counts equal throughout the span?
pg_data %>%
  dplyr::group_by(year) %>%
  dplyr::count(sort = T) %>%
  ungroup()

# where the species counts equal throughout?
pg_data %>%
  group_by(species, year) %>%
  count(sort = T) %>%
  ungroup() %>%
  arrange(species, year)

# Looks like Chinstrap and Gentoo saw some fluctuation

# If they weigh so much, is there a relationship with bill_length, bill_depth_mm,
#   or flipper_length_mm
bill_length <- pg_data %>%
  ggplot(aes(x = bill_length_mm, body_mass_g, color = species)) +
    geom_point()

bill_depth <- pg_data %>%
  ggplot(aes(x = bill_depth_mm, body_mass_g, color = species)) +
  geom_point()

flipper_length <- pg_data %>%
  ggplot(aes(x = flipper_length_mm, body_mass_g, color = species)) +
  geom_point()

(bill_length + bill_depth) /
    flipper_length +
    plot_layout(guides = "collect")
# Overall thoughts:
# Gentoo: thicc penguins, medium bill length, skinnier bill depth, and long flippers
# Chinstrap: lighter than Gentoo, similar to Adelie, medium bill length, deeper bills, and shorter flippers
# Adelie: lighter than Gentoo, short, skinny bills, and short flippers like Chinstraps


# Were males and females evenly distributed throughout the study period?
pg_data %>%
  group_by(year, sex) %>%
  count() %>%
  ungroup() %>%
  arrange(sex, year)
# pretty evenly distributed

# were males and females evenly distributed throughout the species in the study period?
pg_data %>%
  group_by(year, sex, species) %>%
  count() %>%
  ungroup() %>%
  arrange(sex, species, year)


# preprocessing
# sex recipe
sex_recipe <- recipe(sex ~ .,
                     data = pg_data) %>%
  step_naomit(sex) %>%
  step_dummy(island, one_hot = T) %>%
  step_dummy(species, one_hot = T) %>%
  step_string2factor(sex)

sex_prep <- prep(sex_recipe, pg_data)
sex_proc <- bake(sex_prep,
                 new_data = pg_data)

# Train and Test Split
# go 80/20
set.seed(8675309)
sex_split <- initial_split(sex_proc,
                           prop = 0.8,
                           strata = "sex")

sex_train <- training(sex_split)
sex_test <- testing(sex_split)



# Why train on Bootstrapped Data?
# A great way to think about _why_ we use
# this method is to understand the reason
# bootstrap exists is to identify variability
# in your data.  Cross Validation for hyperparameter
# tuning helps with parameter tuning.
# Bootstrapping helps identify the variance
# of the predictors or summary metrics of
# a model.

sex_boot <- bootstraps(sex_train)


# Can we predict the type of sex from the bill length, depth, and flipper length?
# Nearest Neighbors

# logistic regression
# I. Run cv on logistic regression to figure out lambda
logistic_model <- parsnip::logistic_reg() %>%
  set_engine("glm")

penguin_wf <- workflow() %>%
  add_formula(sex ~ .)


glm_rs <-  penguin_wf %>%
  add_model(logistic_model) %>%
  fit_resamples(
      resamples = sex_boot,
      control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )


# Random Forest
rf_model <- parsnip::rand_forest(mode = "classification") %>%
  set_engine("ranger")

rf_rs <- penguin_wf %>%
  add_model(rf_model) %>%
  fit_resamples(
      resamples = sex_boot,
      control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

# metrics
## summary metrics
collect_metrics(glm_rs)

# should be normal-ish
collect_metrics(glm_rs,
                summarize = FALSE) %>%
  group_by(.metric) %>%
  mutate(mean_val = mean(.estimate)) %>%
  ungroup() %>%
  ggplot(aes(x = .estimate, fill = .metric)) +
    geom_histogram(bins = 8) +
    geom_vline(aes(xintercept = mean_val), linetype = "dotted") +
    facet_grid(~ .metric)

# investigating mean and variance
collect_metrics(glm_rs, summarize = FALSE) %>%
  group_by(.metric) %>%
  mutate(mean = mean(.estimate),
         variane = var(.estimate)) %>%
  ungroup()

collect_metrics(rf_rs)

collect_metrics(rf_rs,
                summarize = FALSE) %>%
  group_by(.metric) %>%
  mutate(mean_val = mean(.estimate)) %>%
  ungroup() %>%
  ggplot(aes(x = .estimate, fill = .metric)) +
  geom_histogram(bins = 8) +
  geom_vline(aes(xintercept = mean_val), linetype = "dotted") +
  facet_grid(~ .metric)

collect_metrics(rf_rs, summarize = FALSE) %>%
  group_by(.metric) %>%
  mutate(mean = mean(.estimate),
         variane = var(.estimate)) %>%
  ungroup()

## confusion matrices
glm_rs %>%
  conf_mat_resampled()

rf_rs %>%
  conf_mat_resampled()

## AUC/ROC
glm_rs %>%
  collect_predictions() %>%
    group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(x = 1 - sensitivity, specificity, color = id)) +
    geom_abline(lty = 2, color = "grey80", size = 1.5) +
    geom_path(alpha = 0.4) +
    coord_equal()


## predictions
penguin_final <- penguin_wf %>%
  add_model(logistic_model) %>%
  last_fit(sex_split)

penguin_final

collect_metrics(penguin_final)

collect_predictions(penguin_final)

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = T) %>%
  arrange(estimate)


# Can we predict the type of species from the bill length, depth, and flipper length?
# Multiclass regression

# XGBoost

# Random Forest








# set.seed(8675309)
# species_split <- initial_split(pg_data,
#                           prop = 0.8,
#                           strata = "species")
#
# species_train <- training(species_split)
# species_test <- testing(species_split)


# species_recipe <- recipe(species ~ .,
#                          data = species_train) %>%
#   step_naomit(sex) %>%
#   step_dummy(island, one_hot = T) %>%
#   step_dummy(sex, one_hot = T) %>%
#   step_string2factor(species)
#
# species_prep <- prep(species_recipe, species_train)
# species_proc <- bake(species_prep,
#                      new_data = species_train)
