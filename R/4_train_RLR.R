# 4. TRAIN REGULARIZED LOGISTIC REGRESSION (RLR) ------------------------------

# 1. LOAD LIBRARIES ---------------------------------------------------------
library(caret)
library(dplyr)
library(MLmetrics)
library(glmnet)


# 2. CUSTOM SUMMARY FUNCTION ------------------------------------------------
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "No")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "No")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "No")
  out <- c(Precision = precision, Recall = recall, F1 = f1)
  return(out)
}


# 3. PREPARE BINARY TRAINING DATA -------------------------------------------
train_df_bin <- train_final %>%
  filter(premium_debt_paid_2023 %in% c("1", "2")) %>%
  select(-participant, -year, -split)

# Recode target: "1" = Yes, "2" = No (binary classification)
train_df_bin$premium_debt_paid_2023 <- ifelse(train_df_bin$premium_debt_paid_2023 == "2", "No", "Yes")
train_df_bin$premium_debt_paid_2023 <- factor(train_df_bin$premium_debt_paid_2023, levels = c("No", "Yes"))


# 4. COMPUTE CLASS WEIGHTS --------------------------------------------------
class_counts <- table(train_df_bin$premium_debt_paid_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$premium_debt_paid_2023)]


# 5. SET UP 5-FOLD CV WITH F1 METRICS ---------------------------------------
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)


# 6. HYPERPARAMETER GRID ----------------------------------------------------
tune_grid <- expand.grid(
  alpha = c(0, 0.5, 1),  # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 10)
)


# 7. TRAIN REGULARIZED LOGISTIC REGRESSION ----------------------------------
set.seed(42)
model_rll_f1 <- train(
  premium_debt_paid_2023 ~ .,
  data = train_df_bin,
  method = "glmnet",
  weights = row_weights,
  trControl = cv_control,
  metric = "F1",
  tuneGrid = tune_grid,
  family = "binomial"
)


# 8. REVIEW RESULTS ----------------------------------------------------------
print(model_rll_f1)
best_hyperparams <- model_rll_f1$bestTune
print(best_hyperparams)

cv_results_f1 <- model_rll_f1$resample
print(cv_results_f1)
t.test(cv_results_f1$F1)

