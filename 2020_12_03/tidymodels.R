# install.packages(c("tidymodels", "tidyverse", "ranger", 
# "usemodels", "doParallel", "textrecipes", "doMC", "doFuture"))

library(tidymodels)
library(tidyverse)
library(usemodels)
library(doParallel)
library(textrecipes)
library(doMC)
library(doFuture)

ikea <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv")

ikea %>% 
  select(X1, price, depth:width) %>% 
  pivot_longer(depth:width, names_to = "dim") %>% 
  ggplot(aes(x = value, y = price, color = dim)) +
    geom_point(alpha = 0.4, show.legend = F) +
    scale_y_log10() +
    facet_wrap(~ dim, scales = "free_x") +
    labs(x = NULL)


ikea_df <- ikea %>% 
  select(price, name, category, depth, height, width) %>% 
  mutate(price = log10(price)) %>% 
  mutate_if(is.character, factor)


# model time
set.seed(8675309)
ikea_split <- initial_split(ikea_df, prop = 0.8, strata = price)

ikea_train <- training(ikea_split)
ikea_test <- testing(ikea_split)

# bootstraps
set.seed(42)
ikea_folds <- bootstraps(ikea_train, strata = price)

# used to generate code of the workflow - > WHOA!
use_ranger(price ~ ., data = ikea_train)

ranger_recipe <- recipe(formula = price ~ ., data = ikea_train) %>% 
  step_other(category, name, threshold = 0.01) %>% 
  step_clean_levels(name, category) %>% 
  step_knnimpute(depth, height, width)

ranger_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(17674)
registerDoFuture()
plan(multicore)
ranger_tune <-  tune_grid(ranger_workflow, 
            resamples = ikea_folds, 
            grid = 3)


# This is supposedly working, but not with Tidymodels
# registerDoMC(6)

# x <- iris[which(iris[,5] != "setosa"), c(1,5)]
# trials <- 10000
# ptime <- system.time({
#   r <- foreach(icount(trials), .combine=cbind) %dopar% {
#     ind <- sample(100, 100, replace=TRUE)
#     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
#     coefficients(result1)
#   }
# })[3]
# ptime
# 
# 
# 
# stime <- system.time({
#   r <- foreach(icount(trials), .combine=cbind) %do% {
#     ind <- sample(100, 100, replace=TRUE)
#     result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
#     coefficients(result1)}})[3]
# stime
