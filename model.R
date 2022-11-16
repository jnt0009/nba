library(tidyverse)
library(tidymodels)
library(xgboost)

starters[is.na(starters)] <- 0

starters <- starters |> 
  mutate(
    pos = as.factor(pos)
  )

set.seed(504)



nba_split <- initial_split(starters, strata = pos)
nba_train <- training(nba_split)
nba_test <- testing(nba_split)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), nba_train),
  learn_rate(),
  size = 30
)

nba_folds <- vfold_cv(nba_train, strata = pos)



xgb_wf <- workflow() |>
  add_recipe(rec) |> 
  # add_formula(pos ~ .) |>
  add_model(xgb_spec)


doParallel::registerDoParallel()

set.seed(404)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = nba_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
 
show_notes(.Last.tune.result)

rec <- recipe(pos ~ ., starters) |> 
  update_role(player, new_role = "id") |> 
  step_rm(c("age", "tm", "rk", "g", "gs"))
  


rec |> 
  prep() |> 
  juice()


