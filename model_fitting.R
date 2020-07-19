### Nimon Dong
### Final Project: Model Fitting
### STAT 301-2
### 3/2/2020

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(readr)
library(janitor)
library(skimr)
library(modelr)
library(broom)
library(leaps) # best subset selection
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet
library(pls) # pcr and pls
library(GGally)
library(knitr)
library(MASS)

select <- dplyr::select

# Setting Seed ------------------------------------------------------------

set.seed(3)

# Helper Functions --------------------------------------------------------

# Get predicted values using regsubset object
predict_regsubset <- function(object, fmla , new_data, model_id)
{
  # Not a dataframe? -- handle resample objects/k-folds
  if(!is.data.frame(new_data)){
    new_data <- as_tibble(new_data)
  }
  
  # Get formula
  obj_formula <- as.formula(fmla)
  
  # Extract coefficients for desired model
  coef_vector <- coef(object, model_id)
  
  # Get appropriate feature matrix for new_data
  x_vars <- names(coef_vector)
  mod_mat_new <- model.matrix(obj_formula, new_data)[ , x_vars]
  
  # Get predicted values
  pred <- as.numeric(mod_mat_new %*% coef_vector)
  
  return(pred)
}

# Calculate test MSE on regsubset objects
test_mse_regsubset <- function(object, fmla , test_data){
  
  # Number of models
  num_models <- object %>% summary() %>% pluck("which") %>% dim() %>% .[1]
  
  # Set up storage
  test_mse <- rep(NA, num_models)
  
  # observed targets
  obs_target <- test_data %>% 
    as_tibble() %>% 
    pull(!!as.formula(fmla)[[2]])
  
  # Calculate test MSE for each model class
  for(i in 1:num_models){
    pred <- predict_regsubset(object, fmla, test_data, model_id = i)
    test_mse[i] <- mean((obs_target - pred)^2)
  }
  
  # Return the test errors for each model class
  tibble(model_index = 1:num_models,
         test_mse    = test_mse)
}

# Function to calculate error rate
error_rate_glm <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_win = if_else(pred_prob > 0.5, 1, 0),
           error = pred_win != win) %>% 
    pull(error) %>% 
    mean()
}

error_rate_lda <- function(data, model){
  data %>%
    mutate(pred_win = predict(model, newdata = data) %>%
             pluck("class"),
           error = pred_win != win) %>%
    pull(error) %>%
    mean()
}

confusion_mat_lda <- function(data, model){
  data %>%
    mutate(pred_win = predict(model, newdata = data) %>%
             pluck("class")) %>%
    count(win, pred_win) %>%
    mutate(prop = n / sum(n))
}

# calculating error rate
error_rate_qda <- function(data, model){
  data %>%
    mutate(pred_win = predict(model, newdata = data) %>%
             pluck("class"),
           error = pred_win != win) %>%
    pull(error) %>%
    mean()
}

# generating confusion matrix
confusion_mat_qda <- function(data, model){
  data %>%
    mutate(pred_win = predict(model, newdata = data) %>%
             pluck("class")) %>%
    count(win, pred_win) %>%
    mutate(prop = n / sum(n))
}

knn_tidy <- function(train, test, pred_vars, response_var, ...){
  train_reduced <- train %>% select(!!pred_vars) %>% as.matrix()
  test_reduced  <- test %>% select(!!pred_vars) %>% as.matrix()
  train_class   <- train %>% select(!!response_var) %>% as.matrix()
  
  preds <- class::knn(train = train_reduced, 
                      test = test_reduced, 
                      cl = train_class, ...) 
  
  pred_name <- paste0("pred_", response_var)
  tibble(!!pred_name := preds)
}

# calculating error rate
error_rate_knn <- function(data, pred_value){
  data %>%
    bind_cols(pred_value) %>%
    mutate(error = win != pred_win) %>%
    pull(error) %>%
    mean()
}

# confusion matrix
confusion_mat_knn <- function(data, pred_value){
  data %>%
    bind_cols(pred_value) %>% 
    count(win, pred_win) %>% 
    mutate(prop = n / sum(n))
}

# Importing Dataset -------------------------------------------------------
model_data <- read_csv("data/cleaned_data.csv") %>%
  select(-team_1_name, -team_2_name) %>%
  select(1:36)

# 33 Predictors

model_data_diff <- read_csv("data/cleaned_data.csv") %>%
  select(-team_1_name, -team_2_name) %>%
  select(season, team_1, team_2, win, 37:52) 

# 16 Predictors


# Splitting Dataset: Training / Test --------------------------------------
test_data <- model_data %>% filter(season == 2019)
train_data <- model_data %>% setdiff(test_data)

test_data_diff <- model_data_diff %>% filter(season == 2019)
train_data_diff <- model_data_diff %>% setdiff(test_data_diff)


# Best Subset Selection ---------------------------------------------------
# Due to a large number of predictors in my dataset, best subset selection is not optimal here. This is because the larger the search space, the higher the chance of finding models taht look good on the training data, even though they might not have any predictive power on future data. This leads to overfitting and high variance of the coefficient estimates. We will be using step-wise methods instead as they are much more restrictive and thus a more attractive alternative to best subset selection



# Forward Stepwise Selection ----------------------------------------------
fwd_cv <- train_data %>%
  crossv_kfold(10, id = "folds") %>%
  mutate(fmla = "win ~ . -season -team_1 -team_2",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), data = .y, nvmax = 33, method = "forward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Plot test MSE (forward search)
fwd_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarise(test_mse = mean(test_mse)) %>% 
  arrange(test_mse)

fwd_cv %>%
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarise(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

# Choose 18 predictors 

fwd_cv_diff <- train_data_diff %>%
  crossv_kfold(10, id = "folds") %>%
  mutate(fmla = "win ~ . -season -team_1 -team_2",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), data = .y, nvmax = 16, method = "forward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Plot test MSE (forward search)
fwd_cv_diff %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarise(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) %>%
  kable(digits = 4)

fwd_cv_diff %>%
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarise(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

# Choose 10 predictors 

# Backward Stepwise Selection ---------------------------------------------
back_cv <- train_data %>%
  crossv_kfold(10, id = "folds") %>%
  mutate(fmla = "win ~ . -season -team_1 -team_2",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), data = .y, nvmax = 33, method = "backward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Plot test MSE (backward search)
back_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarise(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) %>%
  kable(digits = 4)

back_cv %>%
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarise(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

# Choose 16 predictors

back_cv_diff <- train_data_diff %>%
  crossv_kfold(10, id = "folds") %>%
  mutate(fmla = "win ~ . -season -team_1 -team_2",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), data = .y, nvmax = 16, method = "backward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Plot test MSE (backward search)
back_cv_diff %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarise(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) %>%
  kable(digits = 4)

back_cv_diff %>%
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarise(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

# Choose 13 predictors

# Inspecting Stepwise Model Coefficients
model_regsubsets <- tibble(train = train_data %>% list(),
                           test = test_data %>% list())

model_regsubsets <- model_regsubsets %>%
  mutate(fwd_selection = map(train, ~ regsubsets(win ~ . -season -team_1 -team_2,
                                                 data = .x, nvmax = 33,
                                                 method = "forward")),
         back_selection = map(train, ~ regsubsets(win ~ . -season -team_1 -team_2,
                                                  data = .x, nvmax = 33,
                                                  method = "backward"))) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

model_regsubsets %>% 
  pluck("fit") %>% 
  map2(c(18, 16), ~ coef(.x, id = .y)) %>% 
  map2(c("fwd", "back"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  kable(digits = 3)

# Stat Differential Model Coefficients
model_regsubsets_diff <- tibble(train = train_data_diff  %>% list(),
                                test = test_data_diff  %>% list())

model_regsubsets_diff  <- model_regsubsets_diff  %>%
  mutate(fwd_selection_diff = map(train, ~ regsubsets(win ~ . -season -team_1 -team_2,
                                                 data = .x, nvmax = 16,
                                                 method = "forward")),
         back_selection_diff = map(train, ~ regsubsets(win ~ . -season -team_1 -team_2,
                                                  data = .x, nvmax = 16,
                                                  method = "backward"))) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

model_regsubsets_diff  %>% 
  pluck("fit") %>% 
  map2(c(10, 13), ~ coef(.x, id = .y)) %>% 
  map2(c("fwd", "back"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  kable(digits = 3)

# Comparing Stepwise Test Errors
regsubset_error <- model_regsubsets %>% 
  mutate(test_mse = map2(fit, test, ~ test_mse_regsubset(.x, win ~ . -season -team_1 -team_2, .y))) %>% 
  unnest(test_mse) %>% 
  group_by(method) %>% 
  filter(model_index == max(model_index)) %>% 
  select(method, test_mse) %>% 
  ungroup() %>%
  arrange(test_mse)

regsubset_error <- hitter_regsubsets %>% 
  mutate(test_mse = map2(fit, test, ~ test_mse_regsubset(.x, salary ~ . -name, .y))) %>% 
  unnest(test_mse) %>% 
  group_by(method) %>% 
  filter(model_index == max(model_index)) %>% 
  select(method, test_mse) %>% 
  ungroup()

regsubset_error_diff  <- model_regsubsets_diff  %>% 
  mutate(test_mse = map2(fit, test, ~ test_mse_regsubset(.x, win ~ . -season -team_1 -team_2, .y))) %>% 
  unnest(test_mse) %>% 
  group_by(method) %>% 
  filter(model_index == max(model_index)) %>% 
  select(method, test_mse) %>% 
  ungroup() %>%
  arrange(test_mse)

regsubset_error
regsubset_error_diff


# Looking at the test MSE's of each stepwise selection method, we will choose fwd_selection_diff -> lowest MSE w/ least # of predictors (10)
# Predictors to use: diff_seed, diff_win_pct, diff_pace, diff_ts_pct, diff_or_pct, diff_ast_pct, diff_stl_pct, diff_ftr, diff_ORtg, diff_DRtg


# Logistic Regression -----------------------------------------------------

# Taking out non-regression columns
model_data_diff <- model_data_diff %>%
  select(-season, -team_1, -team_2)

# Setting up formulas
fmla <- c(
  "win ~ diff_seed",
  "win ~ diff_ORtg + diff_DRtg",
  "win ~ diff_ORtg + diff_DRtg + diff_seed",
  "win ~ diff_efg_pct + diff_to_pct + diff_or_pct + diff_ftr",
  "win ~ diff_seed + diff_win_pct + diff_pace + diff_ts_pct + diff_or_pct + diff_ast_pct + diff_stl_pct + diff_ftr + diff_ORtg + diff_DRtg"
  ) %>% map(as.formula)

model_setup <- tibble(model_name = str_c("log_mod_", seq_along(fmla)),
                      fmla = fmla)

# Adding k-fold and model setup
model_10fold <- model_data_diff %>%
  crossv_kfold(10, id = "fold") %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(model_setup)


# Adding model fit and assessment
glm_10fold <- model_10fold %>%
  mutate(model_fit = map2(fmla, train, glm, family = "binomial"),
         fold_error = map2_dbl(test, model_fit, error_rate_glm))

# Assessing error rates
glm_10fold_error <- glm_10fold %>%
  group_by(model_name) %>%
  summarise(test_error = mean(fold_error)) %>%
  rename(method = model_name,
         test_mse = test_error) %>%
  arrange(test_mse)

glm_10fold_error 

# Best Model -> Model 3 (diff_ORtg + diff_DRtg + diff_seed)


# Linear Discriminant Analysis --------------------------------------------
tib_diff <- tibble(test = list(test_data_diff),
                   train = list(train_data_diff))

lda_fits <- tib_diff %>% 
  mutate(lda_mod_1 = map(train, ~ lda(formula = win ~ diff_seed,
                                   data = .x)),
         lda_mod_2 = map(train, ~ lda(formula = win ~ diff_ORtg + diff_DRtg,
                                   data = .x)),
         lda_mod_3 = map(train, ~ lda(formula = win ~ diff_ORtg + diff_DRtg + diff_seed,
                                   data = .x)),
         lda_mod_4 = map(train, ~ lda(formula = win ~ diff_efg_pct + diff_to_pct + diff_or_pct + diff_ftr,
                                   data = .x)),
         lda_mod_5 = map(train, ~ lda(formula = win ~ diff_seed + diff_win_pct + diff_pace + diff_ts_pct + diff_or_pct + diff_ast_pct + diff_stl_pct + diff_ftr + diff_ORtg + diff_DRtg,
                                   data = .x))) %>% 
  pivot_longer(cols = contains("mod_"), names_to = "model_name", values_to = "model_fit")

lda_fits <- lda_fits %>%
  mutate(train_error = map2_dbl(train, model_fit, error_rate_lda), 
         test_error = map2_dbl(test, model_fit, error_rate_lda), 
         test_confusion = map2(test, model_fit, confusion_mat_lda))

lda_fits_error <- lda_fits %>%
  select(model_name, test_error) %>%
  rename(method = model_name,
         test_mse = test_error) %>%
  arrange(test_mse)

lda_fits_error

# Quadratic Discriminant Analysis -----------------------------------------
qda_fits <- tib_diff %>% 
  mutate(qda_mod_1 = map(train, ~ qda(formula = win ~ diff_seed,
                                       data = .x)),
         qda_mod_2 = map(train, ~ qda(formula = win ~ diff_ORtg + diff_DRtg,
                                       data = .x)),
         qda_mod_3 = map(train, ~ qda(formula = win ~ diff_ORtg + diff_DRtg + diff_seed,
                                       data = .x)),
         qda_mod_4 = map(train, ~ qda(formula = win ~ diff_efg_pct + diff_to_pct + diff_or_pct + diff_ftr,
                                       data = .x)),
         qda_mod_5 = map(train, ~ qda(formula = win ~ diff_seed + diff_win_pct + diff_pace + diff_ts_pct + diff_or_pct + diff_ast_pct + diff_stl_pct + diff_ftr + diff_ORtg + diff_DRtg,
                                       data = .x))) %>% 
  pivot_longer(cols = contains("mod_"), names_to = "model_name", values_to = "model_fit")

qda_fits <- qda_fits %>%
  mutate(train_error = map2_dbl(train, model_fit, error_rate_qda), 
         test_error = map2_dbl(test, model_fit, error_rate_qda), 
         test_confusion = map2(test, model_fit, confusion_mat_qda))

qda_fits_error <- qda_fits %>%
  select(model_name, test_error) %>%
  rename(method = model_name,
         test_mse = test_error) %>%
  arrange(test_mse)

qda_fits_error


# K-Nearest Neighbors -----------------------------------------------------
# Set-up tibble with predictor variables
pred_var <- tibble(pred_set = list(c("diff_seed"), 
                                   c("diff_ORtg", "diff_DRtg"),
                                   c("diff_ORtg", "diff_DRtg", "diff_seed"),
                                   c("diff_efg_pct", "diff_to_pct", "diff_or_pct", "diff_ftr"),
                                   c("diff_seed", "diff_win_pct", "diff_pace", "diff_ts_pct", "diff_or_pct", "diff_ast_pct", 'diff_stl_pct', "diff_ftr", "diff_ORtg", "diff_DRtg")))

# Set-up tibble with num of neighbors (k)
k_values <- tibble(k_value = c(1, 5, 10, 15))

# Set-up tibble with model fitting info & fit to test dataset
knn_fits <- tib_diff %>% 
  crossing(k_values) %>% 
  crossing(pred_var) %>% 
  mutate(knn_preds = pmap(list(train, test, pred_set,"win", k_value),
                          knn_tidy))

# Update knn_fits with error and confusion info
knn_fits <- knn_fits %>% 
  mutate(test_error = map2_dbl(test, knn_preds, error_rate_knn),
         test_confusion = map2(test, knn_preds, confusion_mat_knn))

knn_fits_error <- knn_fits %>% 
  select(pred_set, k_value, test_error) %>% 
  arrange(test_error) %>%
  mutate(pred_count = 1 + str_count(pred_set, pattern = " "),
         method = paste0("knn_mod_", as.character(pred_count), "_", as.character(k_value))) %>%
  select(method, test_error) %>%
  rename(test_mse = test_error)

knn_fits_error


# Ridge Regression & the Lasso --------------------------------------------

# Defining training and test set

test_data <- model_data_diff %>% filter(season == 2019)
train_data <- model_data_diff %>% setdiff(test_data_diff)

# Ridge Regression

lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- train_data %>% 
  cv.glmnet(
    formula = win ~ . -season -team_1 -team_2 -team_1_name -team_2_name, 
    data = ., 
    alpha = 0, 
    nfolds = 10,
    lambda = lambda_grid
  )

plot(ridge_cv)

ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Lasso
lasso_cv <- train_data %>% 
  cv.glmnet(
    formula = win ~ . -season -team_1 -team_2 -team_1_name -team_2_name,  
    data = ., 
    alpha = 1, 
    nfolds = 10
  )

plot(lasso_cv)

lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_min <- lasso_cv$lambda.min

model_glmnet <- tibble(
  train = train_data_diff %>% list(),
  test  = test_data_diff %>% list()
  ) %>%
  mutate(
    ridge_min = map(train, ~ glmnet(win ~ . -season -team_1 -team_2, data = .x,
                                    alpha = 0, lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnet(win ~ . -season -team_1 -team_2, data = .x,
                                    alpha = 0, lambda = ridge_lambda_1se)),
    lasso_min = map(train, ~ glmnet(win ~ . -season -team_1 -team_2, data = .x,
                                    alpha = 1, lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnet(win ~ . -season -team_1 -team_2, data = .x,
                                    alpha = 1, lambda = lasso_lambda_1se))
    ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

model_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y,
         lasso_min = s0.x.x,
         lasso_1se = s0.y.y) %>% 
  knitr::kable(digits = 3)

glmnet_error <- model_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_mse = map2_dbl(test, pred, ~ mean((.x$win - .y)^2))) %>% 
  unnest(test_mse) %>% 
  select(method, test_mse)

glmnet_error

# PCR & PLS Regression ----------------------------------------------------

# PCR Model
pcr_cv <- train_data_diff %>% 
  pcr(win ~ . -season -team_1 -team_2, data = ., scale = TRUE, validation = "CV")

validationplot(pcr_cv, val.type="MSEP")
abline(v = 12)

# PLS Model
pls_cv <- train_data_diff %>% 
  plsr(win ~ . -season -team_1 -team_2, data = ., scale = TRUE, validation = "CV")

validationplot(pls_cv, val.type="MSEP")
abline(v = 4)

# fitting final pcr and pls models
dim_reduct <- tibble(train = train_data_diff %>% list(),
                     test  = test_data_diff %>% list()) %>%
  mutate(pcr_12m = map(train, ~ pcr(win ~ . -season -team_1 -team_2, data = .x, ncomp = 12)),
         pls_4m = map(train, ~ plsr(win ~ . -season -team_1 -team_2, data = .x, ncomp = 4))) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

# Test errors
dim_reduce_error <- dim_reduct %>% 
  mutate(pred = pmap(list(fit, test, c(12, 4)), predict),
         test_mse = map2_dbl(test, pred, ~ mean((.x$win - .y)^2))) %>% 
  unnest(test_mse)  %>% 
  select(method, test_mse)

dim_reduce_error


# Comparing Model Test Errors ---------------------------------------------
regsubset_error %>% 
  bind_rows(regsubset_error_diff) %>%
  bind_rows(glm_10fold_error) %>% 
  bind_rows(lda_fits_error) %>% 
  bind_rows(qda_fits_error) %>% 
  bind_rows(knn_fits_error) %>% 
  bind_rows(glmnet_error) %>% 
  bind_rows(dim_reduce_error) %>% 
  arrange(test_mse) %>%
  kable()












