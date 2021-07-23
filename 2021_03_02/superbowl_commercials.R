source("utils.R")

library(tidymodels)
library(tidyverse)
library(DataExplorer)
library(skimr)
library(tidytext)
library(doMC)
library(doFuture)
library(doParallel)
library(gt)


theme_set(theme_minimal())

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

# can you predict view count? 
youtube_clean <- youtube %>% 
  mutate_if(is.logical, as.numeric) %>% 
  mutate(category_id = as.factor(category_id)) %>% 
  select(-c(superbowl_ads_dot_com_url, youtube_url,
            id, kind, etag, thumbnail, channel_title)) %>% 
  filter(!is.na(view_count))




skimr::skim(youtube_clean)
# ── Data Summary ────────────────────────
# Values       
# Name                       youtube_clean
# Number of rows             247          
# Number of columns          18           
# _______________________                 
# Column type frequency:                  
# character                3            
# factor                   1            
# numeric                  13           
# POSIXct                  1            
# ________________________                
# Group variables            None         
# 
# ── Variable type: character ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable n_missing complete_rate   min   max empty n_unique whitespace
# 1 brand                 0         1         3     9     0       10          0
# 2 title                16         0.935     6    99     0      228          0
# 3 description          50         0.798     3  3527     0      194          0
# 
# ── Variable type: factor ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable n_missing complete_rate ordered n_unique top_counts                   
# 1 category_id          16         0.935 FALSE         13 24: 84, 23: 37, 22: 36, 1: 18
# 
# ── Variable type: numeric ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable        n_missing complete_rate        mean           sd    p0   p25   p50      p75      p100 hist 
# 1 year                         0         1        2010.           5.86   2000  2005  2010   2015        2020 ▇▇▇▇▆
# 2 funny                        0         1           0.692        0.462     0     0     1      1           1 ▃▁▁▁▇
# 3 show_product_quickly         0         1           0.684        0.466     0     0     1      1           1 ▃▁▁▁▇
# 4 patriotic                    0         1           0.166        0.373     0     0     0      0           1 ▇▁▁▁▂
# 5 celebrity                    0         1           0.287        0.453     0     0     0      1           1 ▇▁▁▁▃
# 6 danger                       0         1           0.304        0.461     0     0     0      1           1 ▇▁▁▁▃
# 7 animals                      0         1           0.372        0.484     0     0     0      1           1 ▇▁▁▁▅
# 8 use_sex                      0         1           0.267        0.443     0     0     0      1           1 ▇▁▁▁▃
# 9 view_count                  16         0.935 1407556.    11971111.       10  6431 41379 170016.  176373378 ▇▁▁▁▁
# 10 like_count                  22         0.911    4146.       23920.        0    19   130    527      275362 ▇▁▁▁▁
# 11 dislike_count               22         0.911     834.        6949.        0     1     7     24       92990 ▇▁▁▁▁
# 12 favorite_count              16         0.935       0            0         0     0     0      0           0 ▁▁▇▁▁
# 13 comment_count               25         0.899     189.         986.        0     1    10     50.8      9190 ▇▁▁▁▁
# 
# ── Variable type: POSIXct ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable n_missing complete_rate min                 max                 median              n_unique
# 1 published_at         16         0.935 2006-02-06 10:02:36 2021-01-27 13:11:29 2013-01-31 09:13:55      227

# recipe 
set.seed(69420)
youtube_split <- initial_split(youtube_clean, p = 0.8)

yt_train <- training(youtube_split)
yt_test <- testing(youtube_split)

yt_recipe <- recipe(view_count ~ ., yt_train) %>% 
  step_mutate(month = lubridate::month(published_at),
              day = lubridate::day(published_at)) %>% 
  recipes::step_rm(published_at,
                   title,
                   description) %>% 
  step_string2factor(c(brand, category_id)) %>% 
  step_knnimpute(like_count, dislike_count, favorite_count, comment_count)

yt_prep <- prep(yt_recipe)


final_train <- bake(yt_prep, yt_train)

final_test <- bake(yt_prep, yt_test)


# IT'S MODELLING TIME! 
yt_pois <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>% 
  set_engine("glmnet", family = "poisson")

# get the folds 
set.seed(69420)
yt_folds <- vfold_cv(data = final_train, 10)

# set up a workflow
yt_workflow <- workflow() %>% 
  add_formula(view_count ~ .) %>% 
  add_model(yt_pois)

# GO BRRRRRRRRRRRRRRRRR
doParallel::registerDoParallel(10)


yt_grid <- tune_grid(object = yt_workflow,
          control = control_resamples(save_pred = T, verbose = T),
          resamples = yt_folds,
          grid = 20,
          metrics = metric_set(rmse, mape, rsq))

yt_metrics <- yt_grid %>% 
  collect_metrics() 

yt_metrics %>% 
  mutate(string_mean = case_when(
      .metric %in% c("mape", "rmse") ~ f2si2(mean, rounding = T),
      .metric %in% c("rsq") ~ scales::percent(mean, accuracy = 1.00),
      TRUE ~ "ERRORRR! "
  )) %>% 
  ggplot(aes(x = penalty, y = mixture, color = mean, fill = mean)) +
    geom_point() +
    geom_text(aes(label = string_mean), nudge_x = 0.2) +
    labs(title = "Tune Grid Results") +
    facet_wrap(~ .metric)


yt_best <- yt_grid %>% 
  select_best(metric = "mape")

final_model <-  finalize_model(x = yt_pois, yt_best) %>% 
  fit(formula = view_count ~ ., data = final_train) 


pred <- final_model %>% 
  predict(final_train)

final_train %>% 
  add_column(preds = pred$.pred) %>% 
  ggplot(aes(x = preds, y = view_count)) +
    geom_point() +
    labs(title = "Preds vs. Actuals (view_count)") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    geom_smooth(method = "lm") +
    theme(plot.title.position = "plot")


# save model for posterity 
saveRDS(final_model, 'models/final_lm.rds')
final_lm <- readRDS('models/final_lm.rds')

final_lm %>% 
  predict(new_data = final_test[c(1), ])

# TODO tf-idf as a feature




