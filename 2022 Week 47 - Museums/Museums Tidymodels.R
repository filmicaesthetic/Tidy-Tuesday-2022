## Museums

## https://www.kirenz.com/post/2021-02-17-r-classification-tidymodels/

# get data
pacman::p_load(dplyr, ggplot2, glmnet, tidymodels, ranger, xgboost)
tuesdata <- tidytuesdayR::tt_load(2022, week = 47)
museums <- tuesdata$museums

# convert target variable to factor
museums_df <- museums |>
  rename(tar = Accreditation) |>
  mutate(tar = as.factor(tar))

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible 
set.seed(123)

# Put 3/4 of the data into the training set 
data_split <- initial_split(museums_df, 
                            prop = 3/4, 
                            strata = tar)

# Create dataframes for the two sets:
train_data <- training(data_split) 
test_data <- testing(data_split)

# data to explore
data_explore <- train_data

data_explore |>
  ggplot(aes(x = tar, y = Area_Deprivation_index)) +
  geom_boxplot() +
  geom_point(position = position_jitter(w = 0.3, h = 0.2))

data_explore |>
  ggplot(aes(x = tar, y = Area_Deprivation_index_crime)) +
  geom_boxplot() +
  geom_point(position = position_jitter(w = 0.3, h = 0.2))

data_explore |>
  ggplot(aes(x = tar, y = Area_Deprivation_index_services)) +
  geom_boxplot() +
  geom_point(position = position_jitter(w = 0.3, h = 0.2))

data_explore |>
  filter(Latitude < 80) |>
  ggplot(aes(x = Longitude, y = Latitude)) +
  geom_point(aes(color = tar), alpha = 0.3) +
  theme_minimal() +
  coord_quickmap()

museums_df_new <- museums_df |>
  mutate(Area_Geodemographic_supergroup_code = as.factor(Area_Geodemographic_supergroup_code),
         Area_Geodemographic_subgroup_code = as.factor(Area_Geodemographic_subgroup_code),
         Area_Geodemographic_group_code = as.factor(Area_Geodemographic_group_code),
         Size = as.factor(Size)) |>
  separate(Governance, into = c("Governance_CatA", "Governance_CatB"), sep = "-") |>
  mutate(Governance_CatA = as.factor(Governance_CatA),
         Governance_CatB = as.factor(Governance_CatB)) |>
  separate(Subject_Matter, into = c("Subject_Matter_CatA", "Subject_Matter_CatB"), sep = "-") |>
  mutate(Subject_Matter_CatA = as.factor(Subject_Matter_CatA),
         Subject_Matter_CatB = as.factor(Subject_Matter_CatB)) |>
  select(tar, contains("Area_Deprivation"), 
         Area_Geodemographic_group_code, 
         Area_Geodemographic_subgroup_code, 
         Area_Geodemographic_supergroup_code,
         Size,
         Governance_CatA,
         Governance_CatB,
         Subject_Matter_CatA,
         Subject_Matter_CatB,
         museum_id
         )

set.seed(123)

data_split <- initial_split(museums_df_new, # updated data
                            prop = 3/4, 
                            strata = tar)

train_data <- training(data_split) 
test_data <- testing(data_split)

museums_rec <-
  recipe(tar ~ .,
         data = train_data) %>%
  update_role(museum_id, 
              new_role = "ID") %>%
  step_naomit(everything(), skip = TRUE) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") 

summary(museums_rec)

prepped_data <- 
  museums_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

set.seed(100)

cv_folds <-
  vfold_cv(train_data, 
           v = 5, 
           strata = tar) 

log_spec <- # your model specification
  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 


log_wflow <- # new workflow object
  workflow() %>% # use workflow function
  add_recipe(museums_rec) %>%   # use the new recipe
  add_model(log_spec)   # add your model spec

rf_wflow <-
  workflow() %>%
  add_recipe(museums_rec) %>% 
  add_model(rf_spec) 

xgb_wflow <-
  workflow() %>%
  add_recipe(museums_rec) %>% 
  add_model(xgb_spec)

log_res <- 
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

log_pred <- 
  log_res %>%
  collect_predictions()

log_pred %>% 
  conf_mat(price_category, .pred_class) 

log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")