install.packages(c("tidyverse",
                   "tidymodels",
                   "devtools"))
install.packages("themis")
install.packages("ranger")

library(ranger)
library(themis)
library(tidyverse)
library(tidymodels)
library(devtools)
library(skimr)
devtools::install_github(repo = "nschampions2004/kligon")
library(kligon)

# ggplot theme set
theme_set(theme_minimal())

# data
if (!file.exists("data/trees.csv")) {
  trees_path <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv"
  tree_data <- read_csv(trees_path)
  write_csv(tree_data, "data/trees.csv")
} else {
  tree_data <- read_csv("data/trees.csv")
}

tree_data %>%
  str

# cleaning
tree_parsed <- tree_data %>%
  mutate(legal_status = case_when(legal_status == "DPW Maintained" ~ legal_status,
                                  TRUE ~ "other"),
         plot_size = parse_number(plot_size)) %>%
  select(-address) %>%
  na.omit() %>%
  mutate_if(is.character, as.factor)

# eda
tree_parsed %>%
  skim()


ggplot(tree_parsed, aes(x = longitude, y = latitude, color = legal_status, fill = legal_status)) +
  geom_point(alpha = 0.4) +
  labs(x = NULL, y = NULL,
       title = "How are the legal status of trees distributed across San Fancisco?") +
  theme(plot.title.position = "plot")


tree_parsed %>%
  count(legal_status, caretaker) %>%
  add_count(caretaker, wt = n, name = "caretaker_count") %>%
  filter(caretaker_count > 50) %>%
  group_by(legal_status) %>%
  mutate(percent_legal = n / sum(n)) %>%
  ungroup() %>%
  group_by(caretaker) %>%
  mutate(sum_percent_legal = sum(percent_legal)) %>%
  ungroup() %>%
  mutate(caretaker = fct_reorder(caretaker, sum_percent_legal)) %>%
  ggplot(aes(x = percent_legal, y = caretaker, fill = legal_status)) +
    geom_col(position = "dodge") +
    labs(title = "How does legal_status breakdown over caretaker?",
         subtitle = "Sorted by sum(percent_legal)") +
    theme(plot.title.position = "plot")

# Build Model
## Train/Test Split
set.seed(8675309)
tree_split <- initial_split(tree_parsed, strata = legal_status)

tree_train <- training(tree_split)
tree_test <- testing(tree_split)

tree_recipe <- recipe(legal_status ~ . ,
       tree_train) %>%
  update_role(tree_id, new_role = "ID") %>%
  step_other(species, caretaker, threshold = 0.01) %>%
  step_other(site_info, threshold = 0.005) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_date(date, features = c("year")) %>%
  step_rm(date) %>%
  themis::step_downsample(legal_status)


tree_prep <- prep(tree_recipe)

tree_juiced <- juice(tree_prep)

## Set Model
tree_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")


## define a workflow
tree_workflow <- workflow() %>%
  add_recipe(tree_recipe) %>%
  add_model(tree_spec)

## Hyperparameter Tuning
set.seed(8675309)
tree_fold <- vfold_cv(tree_train, v = 10)

doParallel::registerDoParallel()

set.seed(8675309)
tree_grid <- tune_grid(
  tree_workflow,
  resamples = tree_fold,
  control = control_resamples(save_pred = TRUE, verbose = TRUE),
  grid = 20)

## Results
tree_grid %>%
  collect_metrics()

tree_grid %>%
  collect_metrics(summarize = F)

# ROC Curve
tree_grid %>%
  collect_predictions() %>%
  roc_curve(truth = legal_status, `.pred_DPW Maintained`) %>%
  autoplot()


tree_grid %>%
  select_best("accuracy")

# observing hyperparameter results
tree_grid %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(cols = c(min_n, mtry),
               names_to = "hyperparameter",
               values_to = "value") %>%
  ggplot(aes(x = value, y = mean, color = hyperparameter)) +
    geom_point() +
    facet_wrap(~ hyperparameter, scales = "free_x") +
    labs(title = "How do the Hyperparameters relate to mean accuracy?") +
    theme(plot.title.position = "plot")

tree_grid %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  ggplot(aes(x = mtry, y = min_n, color = mean)) +
    geom_point() +
    labs(title = "How do the Hyperparameters relate to mean accuracy?") +
    scale_color_continuous(type = "viridis") +
    theme(plot.title.position = "plot")

# re-tuning
rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(0,10)),
  levels = 5
)

set.seed(8675309)
honed_tree_grid <- tune_grid(
  tree_workflow,
  resamples = tree_fold,
  control = control_resamples(save_pred = TRUE, verbose = TRUE),
  grid = rf_grid)

# look at new results
honed_tree_grid %>%
  collect_metrics()

honed_tree_grid %>%
  collect_predictions() %>%
  roc_curve(legal_status, `.pred_DPW Maintained`) %>%
  autoplot()

# finalize best model
best_auc <- select_best(honed_tree_grid, "roc_auc")

final_rf <- finalize_model(tree_spec, best_auc)

final_wf <- workflow() %>%
  add_recipe(tree_recipe) %>%
  add_model(final_rf)

final_result <- final_wf %>%
  last_fit(tree_split)

final_result %>%
  collect_metrics()

final_result %>%
  collect_predictions() %>%
  roc_curve(legal_status, `.pred_DPW Maintained`) %>%
  autoplot()

final_result %>%
  collect_predictions() %>%
  bind_cols(tree_test %>% select(-legal_status)) %>%
  mutate(correct = factor(ifelse(legal_status == .pred_class,
                          "Correct",
                          "Incorrect"), levels = c("Incorrect", "Correct"))) %>%
  ggplot(aes(x = longitude, y = latitude, color = correct)) +
    geom_point() +
    labs(title = "Where are the correct predictions located?") +
    theme(plot.title.position = "plot")
