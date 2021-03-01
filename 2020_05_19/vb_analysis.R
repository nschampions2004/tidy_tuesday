# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("devtools")
# install.packages("doParallel")
# install.packages("xgboost")
# devtools::install_github(repo = "nschampions2004/kligon")

library(tidyverse)
library(tidymodels)
library(devtools)
library(kligon)
library(doParallel)
library(xgboost)
library(vip)
# data
if (!file.exists("data/vb_data.csv")) {
  vb_path <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv"
  vb_data <- read_csv(vb_path,
                      guess_max = 76000)
  write_csv(vb_data, "data/vb_data.csv")
} else {
  vb_data <- read_csv("data/vb_data.csv",
                      col_types = "cccdDcdcDddccDddcccDddccDddccctccdddddddddddddddddddddddddddddddd")
}


vb_data %>%
  View()

vb_parsed <- vb_data %>%
  transmute(
    circuit,
    gender,
    year,
    w_attacks = w_p1_tot_attacks + w_p2_tot_attacks,
    w_kills = w_p1_tot_kills + w_p2_tot_kills,
    w_errors = w_p1_tot_errors + w_p2_tot_errors,
    w_aces = w_p1_tot_aces + w_p2_tot_aces,
    w_serve_errors = w_p1_tot_serve_errors + w_p2_tot_serve_errors,
    w_blocks = w_p1_tot_blocks + w_p2_tot_blocks,
    w_digs = w_p1_tot_digs + w_p2_tot_digs,
    l_attacks = l_p1_tot_attacks + l_p2_tot_attacks,
    l_kills = l_p1_tot_kills + l_p2_tot_kills,
    l_errors = l_p1_tot_errors + l_p2_tot_errors,
    l_aces = l_p1_tot_aces + l_p2_tot_aces,
    l_serve_errors = l_p1_tot_serve_errors + l_p2_tot_serve_errors,
    l_blocks = l_p1_tot_blocks + l_p2_tot_blocks,
    l_digs = l_p1_tot_digs + l_p2_tot_digs
  ) %>%
  na.omit()

# separate into winners and losers
winners <- vb_parsed %>%
  select(circuit,gender, year,
         w_attacks:w_digs) %>%
  rename_with(~ str_remove_all(., "w_"), w_attacks:w_digs) %>%
  mutate(win = "win")

losers <- vb_parsed %>%
  select(circuit,gender, year,
         l_attacks:l_digs) %>%
  rename_with(~ str_remove_all(., "l_"), l_attacks:l_digs) %>%
  mutate(win = "lose")

vb_df <- winners %>%
  rbind(losers) %>%
  mutate_if(is.character, as.factor)

# eda plot
vb_df %>%
  pivot_longer(cols = attacks:digs,
               names_to = "stat",
               "values") %>%
  na.omit() %>%
  ggplot(aes(x = gender, y = value, color = win, fill = win)) +
    geom_boxplot(alpha = 0.4) +
    facet_wrap(~ stat, scales = "free_y",
               nrow = 2) +
    labs(y = NULL, color = NULL, fill = NULL)



# train / test split
set.seed(8675309)
vb_split <- initial_split(vb_df, prop = 0.8)

vb_train <- training(vb_split)
vb_test <- testing(vb_split)


# Model Designation
xgb_spec <- boost_tree(
  trees = 1000,
  min_n = tune(),
  tree_depth = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Tuning Parameters
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  # mtry has an infinite positive range, thus
  # we need to finalize it against the training data to
  # determine the max
  finalize(mtry(), vb_train),
  learn_rate(),
  size = 20
)


# Workflow
xgb_workflow <- workflow() %>%
  add_formula(win ~ . )  %>%
  add_model(xgb_spec)


# Tuning Grid on CV
set.seed(8675309)
vb_folds <- vfold_cv(vb_train,
         v = 10,
         strat = win)

doParallel::registerDoParallel()

set.seed(8675309)
xgb_res <- tune_grid(
  object = xgb_workflow,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)


# Results
xgb_res %>%
  collect_metrics() %>%
  View()

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               names_to = "hyperparameter",
               values_to = "value") %>%
  ggplot(aes(x = value, mean, color = hyperparameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~ hyperparameter, scales = "free_x") +
    labs(y = "AUC", x = NULL)

show_best(xgb_res, metric = "roc_auc")

best_auc <- select_best(xgb_res, metric = "roc_auc")

# decision of taking the best auc model
final_xgb <- finalize_workflow(xgb_workflow, best_auc)

final_xgb %>%
  fit(data= vb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

# last fitting
final_result <- last_fit(final_xgb, vb_split)

final_result %>%
  collect_metrics()

final_result %>%
  collect_predictions() %>%
  conf_mat(win, .pred_class)


final_result %>%
  collect_predictions() %>%
  roc_curve(truth = win,
            # based on the documentation
            # this should be the base
            # class of the factor
            .pred_lose) %>%
  autoplot()

# Overall Process:
## Train/Test Split
## Model
## Tuning Parameters
## Workflow
## Tuning Grid on CV
## Decide on Final Model

